(in-package #:doc-extract-backend-docling/tests)

(defun fixture-path ()
  (asdf:system-relative-pathname
   "doc-extract-backend-docling"
   "tests/fixtures/convert-source.json"))

(defun fixture-string ()
  (uiop:read-file-string (fixture-path)))

(defun fixture-backend ()
  (let ((json (fixture-string)))
    (make-docling-backend
     :endpoint "http://docling.test"
     :http-fn (lambda (req)
                (declare (ignore req))
                json))))

(defun capturing-backend (box)
  (let ((json (fixture-string)))
    (make-docling-backend
     :endpoint "http://docling.test:5001"
     :http-fn (lambda (req)
                (setf (car box) req)
                json))))

(deftest json-roundtrip-object
  (let* ((raw "{\"a\":1,\"b\":[true,false,null,\"x\"]}")
         (v (decode-json raw)))
    (ok (hash-table-p v))
    (ok (= 1 (gethash "a" v)))
    (ok (equalp #(t nil :null "x") (gethash "b" v)))
    (ok (search "\"a\":1" (encode-json v)))))

(deftest base64-rfc4648
  (ok (string= "Zg==" (base64-encode "f")))
  (ok (string= "Zm8=" (base64-encode "fo")))
  (ok (string= "Zm9v" (base64-encode "foo")))
  (ok (equalp (map '(vector (unsigned-byte 8)) #'char-code "foo")
              (base64-decode "Zm9v"))))

(deftest parse-canned-docling-document
  (let* ((doc (docling-document->extracted (fixture-string) :format :pdf))
         (secs (document->sections doc))
         (titles (mapcar #'section-title secs)))
    (ok (extracted-document-p doc))
    (ok (string= "Quarterly Review"
                 (document-metadata-title (extracted-document-metadata doc))))
    (ok (string= "quarterly-review.pdf"
                 (document-metadata-filename (extracted-document-metadata doc))))
    (ok (= 3 (length secs)))
    (ok (equal '("Quarterly Review" "Summary" "Metrics") titles))
    (ok (= 1 (section-level (first secs))))
    (ok (= 2 (section-level (second secs))))
    (ok (search "Q3 results" (section-text (second secs))))
    (ok (search "Revenue" (section-text (third secs))))
    (ok (search "Revenue" (document-text doc)))
    (ok (= 1 (length (extracted-document-pages doc))))))

(deftest mock-http-fn-extract-document
  (let* ((b (fixture-backend))
         (doc (extract-document b "not-a-real-file" :format :pdf)))
    (ok (extracted-document-p doc))
    (ok (string= "Quarterly Review"
                 (document-metadata-title (extracted-document-metadata doc))))
    (ok (search "Q3 results" (document-text doc)))
    (let ((table (find-if (lambda (blk)
                            (typep blk 'table-block))
                          (block-children
                           (find-if (lambda (s)
                                      (and (typep s 'section)
                                           (string= "Metrics" (section-title s))))
                                    (block-children (first (extracted-document-blocks doc))))))))
      (ok (typep table 'table-block))
      (ok (= 3 (table-block-rows table)))
      (ok (= 2 (table-block-cols table))))))

(deftest extract-sections-via-document
  (let ((secs (extract-sections (fixture-backend) "x.pdf" :format :pdf)))
    (ok (= 3 (length secs)))
    (ok (every #'extracted-section-p secs))
    (ok (string= "Summary" (section-title (second secs))))
    (ok (search "Q3" (section-text (second secs))))))

(deftest extract-text-and-metadata
  (let ((b (fixture-backend)))
    (ok (search "Headcount" (extract-text b "x.pdf" :format :pdf)))
    (let ((md (extract-metadata b "x.pdf" :format :pdf)))
      (ok (eq :pdf (getf md :format)))
      (ok (string= "Quarterly Review" (getf md :title)))
      (ok (string= "quarterly-review.pdf" (getf md :filename))))))

(deftest convert-request-shape
  (let* ((box (list nil))
         (b (capturing-backend box)))
    (extract-document b "hello" :format :html)
    (let ((req (car box)))
      (ok (or (docling-http-request-p req)
              (not (null req))))
      (when (docling-http-request-p req)
        (ok (eq :post (docling-http-request-method req)))
        (ok (search "/v1/convert/source" (docling-http-request-url req)))
        (ok (search "docling.test:5001" (docling-http-request-url req)))
        (ok (search "\"kind\":\"file\"" (docling-http-request-body req)))
        (ok (search "base64_string" (docling-http-request-body req)))
        (ok (search "document.html" (docling-http-request-body req)))))))

(deftest missing-http-fn-signals
  (let ((b (make-docling-backend :endpoint "http://docling.test")))
    (ok (signals (extract-document b "x" :format :pdf)
                 'doc-extract-error))))

(deftest use-value-restart-without-http-fn
  (let* ((b (make-docling-backend))
         (doc (handler-bind
                  ((doc-extract-error
                    (lambda (c)
                      (declare (ignore c))
                      (use-value (fixture-string)))))
                (extract-document b "x" :format :pdf))))
    (ok (extracted-document-p doc))
    (ok (string= "Quarterly Review"
                 (document-metadata-title (extracted-document-metadata doc))))))

(deftest http-error-status
  (let ((b (make-docling-backend
            :http-fn (lambda (req)
                       (declare (ignore req))
                       '(:status 503 :body "{\"status\":\"failure\"}")))))
    (ok (signals (extract-document b "x" :format :pdf)
                 'doc-extract-error))))

(deftest response-primitives-before-http-protocol
  ;; CI loads http-protocol as a transitive dep. Mock fns return a JSON
  ;; string or a keyword plist — never call RESPONSE-STATUS on those.
  (ok (= 200 (doc-extract-backend-docling::%response-status
              (fixture-string))))
  (ok (= 503 (doc-extract-backend-docling::%response-status
              '(:status 503 :body "{\"status\":\"failure\"}"))))
  (ok (string= "{\"status\":\"failure\"}"
               (doc-extract-backend-docling::%response-body
                '(:status 503 :body "{\"status\":\"failure\"}")))))

(deftest merged-region-uses-protocol-class
  (let* ((raw "{\"schema_name\":\"DoclingDocument\",\"name\":\"span\",\"body\":{\"children\":[{\"$ref\":\"#/tables/0\"}]},\"texts\":[],\"pictures\":[],\"tables\":[{\"self_ref\":\"#/tables/0\",\"label\":\"table\",\"data\":{\"num_rows\":2,\"num_cols\":1,\"table_cells\":[{\"start_row_offset_idx\":0,\"start_col_offset_idx\":0,\"row_span\":2,\"col_span\":1,\"text\":\"A\"}]}}]}")
         (doc (docling-document->extracted raw))
         (table (find-if (lambda (b) (typep b 'table-block))
                         (extracted-document-blocks doc))))
    (ok (typep table 'table-block))
    (ok (= 1 (length (table-block-merged table))))
    (ok (typep (first (table-block-merged table)) 'merged-region))
    (ok (= 2 (merged-region-row-span (first (table-block-merged table)))))))

(deftest self-register-priority
  (let ((ext (find-extractor :pdf)))
    (ok (typep ext 'docling-backend)))
  (ok (typep (find-extractor :docx) 'docling-backend))
  (ok (typep (find-extractor :html) 'docling-backend))
  (ok (typep (find-extractor "application/pdf") 'docling-backend)))

(deftest live-docling-serve
  (let ((url (uiop:getenv "DOC_EXTRACT_DOCLING_URL")))
    (cond
      ((or (null url) (zerop (length url)))
       (skip "DOC_EXTRACT_DOCLING_URL not set — live canary skipped"))
      ((not (zerop (nth-value 2 (uiop:run-program '("curl" "--version")
                                                   :ignore-error-status t
                                                   :output :string
                                                   :error-output :string))))
       (skip "curl not on PATH — live canary skipped"))
      (t
       (let* ((html "<html><h1>Quarterly Review</h1><h2>Summary</h2><p>The board reviewed Q3 results.</p><h2>Metrics</h2><table><tr><th>Metric</th><th>Value</th></tr><tr><td>Revenue</td><td>12</td></tr></table></html>")
              (b (make-docling-backend
                  :endpoint url
                  :http-fn (lambda (req)
                             (uiop:run-program
                              (list "curl" "-sS" "-X" "POST"
                                    "-H" "Content-Type: application/json"
                                    "--max-time"
                                    (princ-to-string
                                     (or (docling-http-request-timeout req) 120))
                                    "--data-binary" "@-"
                                    (docling-http-request-url req))
                              :input (make-string-input-stream
                                      (docling-http-request-body req))
                              :output :string
                              :error-output :string))))
              (doc (extract-document b html :format :html)))
         (ok (extracted-document-p doc))
         (ok (plusp (length (document-text doc)))))))))
