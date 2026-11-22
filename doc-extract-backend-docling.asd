(defsystem "doc-extract-backend-docling"
  :version "0.1.1"
  :description "docling-serve HTTP backend for doc-extract-protocol (injectable http-fn)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "doc-extract-protocol" "0.3.1")
               (:version "encoding-protocol" "0.3.0")
               "json-protocol" "json-backend-jzon")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "json")
               (:file "backend"))
  :in-order-to ((test-op (test-op "doc-extract-backend-docling/tests"))))

(defsystem "doc-extract-backend-docling/tests"
  :depends-on ("doc-extract-backend-docling" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
