(in-package #:doc-extract-backend-docling)

;;; docling-serve v1 (docling-project/docling-serve ≥ 1.0).
;;; Convert path is POST /v1/convert/source with a unified `sources` array
;;; (`kind: file` + `base64_string`). The v0 / v1alpha `file_sources` body
;;; is gone. Multipart POST /v1/convert/file exists but is not used here.

(defparameter +convert-source-path+ "/v1/convert/source"
  "Pinned docling-serve v1 convert path (JSON + base64 file source).")

(defparameter +docling-priority+ 40
  "Self-register priority. Colocated HTML/DOCX extractors use 10; corporate
   profiles want docling above pdfium/unstructured. Re-register to override.")

(defparameter +docling-formats+
  '(:pdf :docx :html
    "application/pdf"
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    "text/html")
  "Formats this backend self-registers for.")

(defparameter *default-docling-endpoint* "http://127.0.0.1:5001")

(defstruct docling-http-request
  method
  url
  headers
  body
  timeout)

(defclass docling-backend (doc-extract-backend)
  ((endpoint :initarg :endpoint :accessor docling-backend-endpoint
             :initform nil)
   (timeout :initarg :timeout :accessor docling-backend-timeout
            :initform 120)
   (http-fn :initarg :http-fn :accessor docling-backend-http-fn
            :initform nil))
  (:documentation
   "doc-extract-protocol backend over docling-serve. HTTP is injected
    via HTTP-FN — do not hard-depend on a specific client (S3 pattern)."))

(defun %env-endpoint ()
  (let ((e (uiop:getenv "DOC_EXTRACT_DOCLING_URL")))
    (and e (plusp (length e)) (string-right-trim '(#\/) e))))

(defun make-docling-backend (&key endpoint timeout http-fn)
  (make-instance 'docling-backend
                 :endpoint (or endpoint (%env-endpoint) *default-docling-endpoint*)
                 :timeout (or timeout 120)
                 :http-fn http-fn))

(defun use-docling-backend (&rest args &key &allow-other-keys)
  (setf doc-extract-protocol:*doc-extract-backend*
        (apply #'make-docling-backend args)))

(defun convert-url (backend)
  (format nil "~a~a"
          (string-right-trim '(#\/) (or (docling-backend-endpoint backend)
                                        *default-docling-endpoint*))
          +convert-source-path+))

(defun %mimetype (format)
  (case (canonicalize-format format)
    (:pdf "application/pdf")
    (:docx "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    (:html "text/html")
    (t nil)))

(defun %coerce-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8))
     (make-array (length source)
                 :element-type '(unsigned-byte 8)
                 :initial-contents source))
    (pathname
     (with-open-file (in source :element-type '(unsigned-byte 8)
                                :if-does-not-exist :error)
       (let ((buf (make-array (file-length in)
                              :element-type '(unsigned-byte 8))))
         (read-sequence buf in)
         buf)))
    (string
     (if (and (not (find #\Newline source))
              (probe-file source))
         (%coerce-octets (pathname source))
         (coerce (encoding-protocol:encode source :encoding :utf-8)
                 '(simple-array (unsigned-byte 8) (*)))))))

(defun %filename (source format)
  (cond
    ((pathnamep source) (file-namestring source))
    ((and (stringp source)
          (not (find #\Newline source))
          (probe-file source))
     (file-namestring (pathname source)))
    (t
     (format nil "document.~a"
             (string-downcase
              (symbol-name (or (canonicalize-format format) :pdf)))))))

(defun build-convert-request (backend source &key format)
  "Build the POST /v1/convert/source request (does not send it)."
  (let* ((octets (%coerce-octets source))
         (filename (%filename source format))
         (body (encode-json
                `(("options" . (("to_formats" . #("json"))))
                  ("sources" . #((("kind" . "file")
                                  ("base64_string" . ,(base64-encode octets))
                                  ("filename" . ,filename))))))))
    (make-docling-http-request
     :method :post
     :url (convert-url backend)
     :headers '(("content-type" . "application/json"))
     :body body
     :timeout (docling-backend-timeout backend))))

(defun %plist-response-p (response)
  (and (consp response) (keywordp (car response))))

(defun %soft-http-call (name response)
  "Call http-protocol GF only on CLOS responses. Never on strings/plists
   (CI loads http-protocol as a transitive dep; mocks return JSON/plists)."
  (when (typep response 'standard-object)
    (let* ((pkg (find-package :http-protocol))
           (fn (and pkg (find-symbol name pkg))))
      (when (and fn (fboundp fn))
        (handler-case (funcall fn response)
          (error () nil))))))

(defun %response-status (response)
  (cond
    ((integerp response) response)
    ((or (stringp response)
         (hash-table-p response)
         (and (vectorp response) (not (stringp response))))
     200)
    ((%plist-response-p response)
     (or (getf response :status) 200))
    ((and (consp response) (integerp (car response)))
     (car response))
    (t
     (or (%soft-http-call "RESPONSE-STATUS" response) 200))))

(defun %response-body (response)
  (cond
    ((or (stringp response) (hash-table-p response)) response)
    ((and (vectorp response) (not (stringp response))) response)
    ((%plist-response-p response)
     (getf response :body))
    ((and (consp response) (integerp (car response)))
     (second response))
    (t
     (or (%soft-http-call "RESPONSE-BODY" response) response))))

(defun %invoke-http (backend req)
  (let ((fn (docling-backend-http-fn backend)))
    (unless fn
      (error 'doc-extract-error
             :message "docling-backend has no http-fn — inject one or load an HTTP client"))
    ;; Pass the struct. http-fn is injected; do not coerce through
    ;; http-protocol:MAKE-HTTP-REQUEST (that system may be loaded in CI
    ;; without the fn expecting its request class).
    (funcall fn req)))

(defun jget (obj key)
  (let ((s (if (stringp key) key (string key))))
    (cond
      ((hash-table-p obj)
       (or (gethash s obj)
           (gethash (string-downcase s) obj)))
      ((and (consp obj) (consp (car obj)))
       (cdr (assoc s obj :test #'string-equal)))
      (t nil))))

(defun jstr (v)
  (cond
    ((stringp v) v)
    ((or (null v) (eq v :null)) "")
    (t (princ-to-string v))))

(defun jint (v &optional default)
  (cond
    ((integerp v) v)
    ((realp v) (truncate v))
    ((stringp v) (or (parse-integer v :junk-allowed t) default))
    (t default)))

(defun jnum (v &optional default)
  (cond
    ((realp v) v)
    ((stringp v)
     (let ((*read-eval* nil))
       (or (ignore-errors (read-from-string v)) default)))
    (t default)))

(defun jvec (v)
  (cond
    ((null v) #())
    ((eq v :null) #())
    ((and (vectorp v) (not (stringp v))) v)
    ((listp v) (coerce v 'vector))
    (t (vector v))))

(defun jlist (v)
  (coerce (jvec v) 'list))

(defun %truthy (v)
  (and v (not (eq v :null))))

(defun %unwrap-document (raw)
  (cond
    ((jget raw "json_content")
     (jget raw "json_content"))
    ((jget raw "document")
     (%unwrap-document (jget raw "document")))
    (t raw)))

(defun parse-docling-response (source)
  "Parse a docling-serve convert JSON (string/octets/table) to a DoclingDocument table."
  (let ((raw (if (or (hash-table-p source)
                     (and (consp source) (not (stringp source))))
                 source
                 (decode-json source))))
    (let ((status (jstr (jget raw "status"))))
      (when (and (plusp (length status))
                 (not (member status '("success" "partial_success")
                              :test #'string-equal)))
        (error 'doc-extract-error
               :message (format nil "docling-serve status ~a" status))))
    (%unwrap-document raw)))

(defun %ref-pointer (ref)
  (cond
    ((stringp ref) ref)
    ((hash-table-p ref) (jget ref "$ref"))
    ((and (consp ref) (consp (car ref)))
     (cdr (assoc "$ref" ref :test #'string=)))
    (t nil)))

(defun %index-items (index prefix items)
  (loop for item across (jvec items)
        for i from 0
        do (let ((ptr (let ((sr (jstr (jget item "self_ref"))))
                        (if (plusp (length sr))
                            sr
                            (format nil "#/~a/~d" prefix i)))))
             (setf (gethash ptr index) item))))

(defun item-text (item)
  (let ((t1 (jstr (jget item "text"))))
    (if (plusp (length t1))
        t1
        (jstr (jget item "orig")))))

(defun furniture-p (item)
  (member (string-downcase (jstr (jget item "content_layer")))
          '("furniture" "header" "footer")
          :test #'string=))

(defun item-layer (item)
  (if (furniture-p item) :furniture :body))

(defun item-prov (item)
  (loop for p in (jlist (jget item "prov"))
        collect (make-provenance-entry
                 :page (jint (or (jget p "page_no") (jget p "page")))
                 :bbox (let ((b (jget p "bbox")))
                         (when b
                           (list (jnum (jget b "l"))
                                 (jnum (jget b "t"))
                                 (jnum (jget b "r"))
                                 (jnum (jget b "b"))))))))

(defun text-kind (label)
  (cond
    ((member label '("caption") :test #'string=) :caption)
    ((member label '("footnote") :test #'string=) :footnote)
    ((member label '("formula") :test #'string=) :formula)
    ((member label '("quote") :test #'string=) :quote)
    (t :para)))

(defun caption-text (item index)
  (loop for ref in (jlist (jget item "captions"))
        for cap = (gethash (%ref-pointer ref) index)
        for text = (and cap (item-text cap))
        when (and text (plusp (length text)))
          return text))

(defun table-item->block (item)
  (let* ((data (or (jget item "data") item))
         (rows (or (jint (jget data "num_rows")) 0))
         (cols (or (jint (jget data "num_cols")) 0))
         (cells '())
         (merged '()))
    (dolist (c (jlist (jget data "table_cells")))
      (let* ((r (or (jint (jget c "start_row_offset_idx")) 0))
             (col (or (jint (jget c "start_col_offset_idx")) 0))
             (rs (max 1 (or (jint (jget c "row_span")) 1)))
             (cs (max 1 (or (jint (jget c "col_span")) 1)))
             (text (jstr (jget c "text"))))
        (push (make-table-cell
               :row r :col col :row-span rs :col-span cs
               :header-p (or (%truthy (jget c "column_header"))
                             (%truthy (jget c "row_header")))
               :content (when (plusp (length text))
                          (list (make-text-block :text text))))
              cells)
        (when (or (> rs 1) (> cs 1))
          ;; protocol exports MERGED-REGION, not MAKE-MERGED-REGION
          (push (make-instance 'merged-region
                               :row r :col col
                               :row-span rs :col-span cs)
                merged))))
    (make-table-block :rows rows :cols cols
                      :cells (nreverse cells)
                      :merged (nreverse merged)
                      :layer (item-layer item)
                      :provenance (item-prov item))))

(defun picture-item->block (item index)
  (let* ((image (jget item "image"))
         (uri (and image (jstr (jget image "uri"))))
         (alt (or (caption-text item index) (item-text item)))
         (octets nil)
         (marker (and uri (search "base64," uri))))
    (when marker
      (setf octets (ignore-errors (base64-decode (subseq uri (+ marker 7))))))
    (make-image-block :alt (and (plusp (length alt)) alt)
                      :ocr-text (let ((t1 (item-text item)))
                                  (and (plusp (length t1)) t1))
                      :octets octets
                      :layer (item-layer item)
                      :provenance (item-prov item))))

(defun walk-refs (refs index)
  (loop for ref in (jlist refs)
        for item = (gethash (%ref-pointer ref) index)
        when item
          append (item-blocks item index)))

(defun item-blocks (item index)
  (let ((label (string-downcase (jstr (jget item "label")))))
    (cond
      ((member label '("list" "ordered_list") :test #'string=)
       (list (make-list-block
              :ordered-p (or (string= label "ordered_list")
                             (%truthy (jget item "enumerated")))
              :items (walk-refs (jget item "children") index)
              :layer (item-layer item))))
      ((and (not (plusp (length (jstr (jget item "text")))))
            (jget item "children")
            (member label '("unspecified" "section" "chapter" "")
                    :test #'string=))
       (walk-refs (jget item "children") index))
      ((member label '("title" "section_header") :test #'string=)
       (list (make-section-block
              :title (item-text item)
              :level (or (jint (jget item "level"))
                         (if (string= label "title") 1 2))
              :children (walk-refs (jget item "children") index)
              :layer (item-layer item)
              :provenance (item-prov item))))
      ((string= label "code")
       (list (make-code-block :text (item-text item)
                              :layer (item-layer item))))
      ((string= label "table")
       (list (table-item->block item)))
      ((string= label "picture")
       (list (picture-item->block item index)))
      ((member label '("key_value_item" "key_value") :test #'string=)
       (list (make-key-value-block
              :key (jstr (or (jget item "key_text")
                             (jget (jget item "key") "text")
                             (jget item "key")))
              :value (jstr (or (jget item "value_text")
                               (jget (jget item "value") "text")
                               (jget item "value")))
              :layer (item-layer item))))
      (t
       (list (make-text-block
              :kind (text-kind label)
              :text (item-text item)
              :layer (item-layer item)
              :provenance (item-prov item)))))))

(defun nest-sections (blocks)
  "Nest a sibling list by heading level. Pre-existing section children stay."
  (let ((roots '())
        (stack '()))
    (flet ((add-child (parent child)
             (setf (section-children parent)
                   (append (or (section-children parent) nil)
                           (list child)))))
      (dolist (b blocks)
        (if (typep b 'section)
            (let ((level (or (section-level b) 1)))
              (loop while (and stack (>= (car (first stack)) level))
                    do (pop stack))
              (if stack
                  (add-child (cdr (first stack)) b)
                  (push b roots))
              (push (cons level b) stack))
            (if stack
                (add-child (cdr (first stack)) b)
                (push b roots)))))
    (nreverse roots)))

(defun first-title (texts)
  (loop for item across (jvec texts)
        for label = (string-downcase (jstr (jget item "label")))
        when (member label '("title" "section_header") :test #'string=)
          return (item-text item)))

(defun parse-pages (pages)
  (cond
    ((or (null pages) (eq pages :null)) nil)
    ((hash-table-p pages)
     (let ((acc '()))
       (maphash (lambda (k v)
                  (push (make-page-info
                         :number (or (jint (jget v "page_no"))
                                     (jint k)
                                     1)
                         :width (jnum (or (jget (jget v "size") "width")
                                          (jget v "width")))
                         :height (jnum (or (jget (jget v "size") "height")
                                           (jget v "height"))))
                        acc))
                pages)
       (sort acc #'< :key #'page-info-number)))
    (t
     (loop for v in (jlist pages)
           collect (make-page-info
                    :number (or (jint (jget v "page_no"))
                                (jint (jget v "number"))
                                1)
                    :width (jnum (or (jget (jget v "size") "width")
                                     (jget v "width")))
                    :height (jnum (or (jget (jget v "size") "height")
                                      (jget v "height"))))))))

(defun docling-document->extracted (raw &key format)
  "Map a DoclingDocument (or convert wrapper) to EXTRACTED-DOCUMENT."
  (let* ((dd (if (and (or (hash-table-p raw) (consp raw))
                      (or (jget raw "texts")
                          (jget raw "body")
                          (string-equal (jstr (jget raw "schema_name"))
                                        "DoclingDocument")))
                 raw
                 (parse-docling-response raw)))
         (index (make-hash-table :test #'equal))
         (origin (jget dd "origin"))
         (texts (jget dd "texts")))
    (%index-items index "texts" texts)
    (%index-items index "tables" (jget dd "tables"))
    (%index-items index "pictures" (jget dd "pictures"))
    (%index-items index "groups" (jget dd "groups"))
    (%index-items index "key_value_items" (jget dd "key_value_items"))
    (let* ((body (jget dd "body"))
           (blocks (nest-sections
                    (walk-refs (and body (jget body "children")) index)))
           (title (or (first-title texts) (jstr (jget dd "name"))))
           (doc (make-extracted-document
                 :metadata (make-document-metadata
                            :title (and (plusp (length title)) title)
                            :filename (let ((fn (jstr (jget origin "filename"))))
                                        (and (plusp (length fn)) fn))
                            :mimetype (let ((mt (jstr (jget origin "mimetype"))))
                                        (or (and (plusp (length mt)) mt)
                                            (%mimetype format)))
                            :content-hash (let ((h (jstr (jget origin "binary_hash"))))
                                            (and (plusp (length h)) h)))
                 :pages (parse-pages (jget dd "pages"))
                 :blocks blocks)))
      (ensure-ids doc)
      doc)))

(defun %section-plain (sec)
  (with-output-to-string (s)
    (let ((first t))
      (dolist (c (or (section-children sec) nil))
        (unless (typep c 'section)
          (let ((txt (block-plain-text c)))
            (when (plusp (length txt))
              (unless first (write-char #\Newline s))
              (setf first nil)
              (write-string txt s))))))))

(defun document->sections (doc)
  "EXTRACTED-DOCUMENT → 0.1 EXTRACTED-SECTION list (title / text / level)."
  (check-type doc extracted-document)
  (let ((out '()))
    (labels ((walk (blocks)
               (dolist (b blocks)
                 (when (typep b 'section)
                   (push (make-extracted-section
                          :title (or (section-title b) "")
                          :level (or (section-level b) 1)
                          :text (%section-plain b))
                         out)
                   (walk (section-children b))))))
      (walk (extracted-document-blocks doc))
      (let ((secs (nreverse out)))
        (if secs
            secs
            (list (make-extracted-section
                   :title (or (and (extracted-document-metadata doc)
                                   (document-metadata-title
                                    (extracted-document-metadata doc)))
                              "")
                   :text (document-text doc)
                   :level 1)))))))

(defun %convert (backend source format)
  (tagbody
   :again
     (restart-case
         (let* ((req (build-convert-request backend source :format format))
                (resp (%invoke-http backend req))
                (status (%response-status resp))
                (body (%response-body resp)))
           (when (and (integerp status) (>= status 400))
             (error 'doc-extract-error
                    :format format
                    :message (format nil "docling-serve HTTP ~a" status)))
           (return-from %convert (parse-docling-response body)))
       (use-value (value)
         :report "Use a supplied DoclingDocument / convert JSON"
         :interactive (lambda ()
                        (format *query-io* "Value: ")
                        (force-output *query-io*)
                        (list (read *query-io*)))
         (return-from %convert
           (if (or (hash-table-p value) (stringp value))
               (parse-docling-response value)
               value)))
       (retry ()
         :report "Retry the docling-serve convert request"
         (go :again)))))

(defmethod extract-document ((backend docling-backend) source &key format)
  (docling-document->extracted (%convert backend source format) :format format))

(defmethod extract-sections ((backend docling-backend) source &key format)
  (document->sections (extract-document backend source :format format)))

(defmethod extract-text ((backend docling-backend) source &key format)
  (document-text (extract-document backend source :format format)))

(defmethod extract-metadata ((backend docling-backend) source &key format)
  (let* ((doc (extract-document backend source :format format))
         (md (extracted-document-metadata doc)))
    (append (list :format (or (canonicalize-format format) :pdf))
            (when (and md (document-metadata-title md))
              (list :title (document-metadata-title md)))
            (when (and md (document-metadata-filename md))
              (list :filename (document-metadata-filename md)))
            (when (and md (document-metadata-mimetype md))
              (list :mimetype (document-metadata-mimetype md))))))

(defmethod extract-metadata :around ((backend docling-backend) source
                                     &key format)
  (restart-case (call-next-method)
    (use-value (value)
      :report "Use a supplied metadata plist"
      value)
    (retry ()
      :report "Retry extract-metadata"
      (extract-metadata backend source :format format))))

(defmethod extract-sections :around ((backend docling-backend) source
                                     &key format)
  (restart-case (call-next-method)
    (use-value (value)
      :report "Use a supplied section list"
      value)
    (retry ()
      :report "Retry extract-sections"
      (extract-sections backend source :format format))))

(register-extractor 'docling-backend
                    :formats +docling-formats+
                    :priority +docling-priority+)
