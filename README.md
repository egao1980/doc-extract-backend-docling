# doc-extract-backend-docling

[`doc-extract-protocol`](https://github.com/egao1980/doc-extract-protocol) backend that POSTs a base64 file source to self-hosted **[docling-serve](https://github.com/docling-project/docling-serve)** and maps the DoclingDocument JSON (`texts` / `tables` / `pictures` + `body` tree) onto the C3d `extracted-document` block tree.

HTTP is **injected** (`http-fn` slot) — this system does **not** depend on a specific HTTP client (`object-store-backend-s3` pattern).

```lisp
(asdf:load-system "doc-extract-backend-docling")

(let ((b (doc-extract-backend-docling:make-docling-backend
          :endpoint "http://127.0.0.1:5001"
          :http-fn (lambda (req)
                     ;; REQ is a DOCLING-HTTP-REQUEST (or http-protocol request)
                     (your-http-post req)))))
  (doc-extract-protocol:extract-document b #p"memo.pdf" :format :pdf))
```

## Pinned convert API (docling-serve v1)

docling-serve moved off the prototype `v1alpha` / `file_sources` body. This backend pins:

| | |
|--|--|
| Method / path | **`POST /v1/convert/source`** |
| Content-Type | `application/json` |
| Body | `{ "options": { "to_formats": ["json"] }, "sources": [{ "kind": "file", "base64_string": "…", "filename": "doc.pdf" }] }` |
| Response | `{ "document": { "json_content": { … DoclingDocument … } }, "status": "success" }` |

`POST /v1/convert/file` (multipart) exists on the server and is **not** used here. Do not send the old `file_sources` / `http_sources` keys — current v1.x rejects them.

Default endpoint: `http://127.0.0.1:5001`, or `DOC_EXTRACT_DOCLING_URL` when set. Timeout slot default: 120s (passed through on the request object).

## Extractor mapping

| Binding | Role |
|---------|------|
| `docling-backend` | CLOS class (`doc-extract-backend`) |
| `make-docling-backend` | `&key endpoint timeout http-fn` |
| `use-docling-backend` | bind `*doc-extract-backend*` |
| `extract-document` | primary GF → `extracted-document` |
| `extract-sections` | derived from the document (`document->sections`) |
| `extract-text` / `extract-metadata` | `document-text` / metadata plist |

Self-registers at load via `register-extractor` for **`:pdf` `:docx` `:html`** (plus their MIME types) at priority **`40`**.

| Backend | Typical priority | Notes |
|---------|------------------|--------|
| colocated HTML / DOCX (`default-doc-extract-backend`) | 10 | personal / no service |
| `doc-extract-backend-pdf` (pdfium) | (when registered) below 40 | native PDF |
| **docling** | **40** | corporate default for PDF / DOCX / HTML |
| unstructured | (sibling C3c) between 10 and 40 unless re-registered | broader formats |

Profiles re-register to change the winner — there is no second selection mechanism. `find-extractor` instantiates `docling-backend` with the default endpoint; inject `http-fn` on the instance before calling the GFs.

DoclingDocument → C3d:

- `title` / `section_header` → `section` (nest by `level` / body tree)
- `paragraph` / `text` / `caption` / `footnote` → `text-block`
- `code` → `code-block`
- `list` / `ordered_list` groups → `list-block`
- `tables[].data.table_cells` → `table-block` (+ `merged-region` on spans)
- `pictures` → `image-block` (alt from caption; data-URI octets when present)
- `content_layer: furniture` → `:furniture`
- `origin` / `pages` → document metadata + `page-info`

## Tests

`asdf:test-system "doc-extract-backend-docling"` is always-on against `tests/fixtures/convert-source.json` (mock `http-fn`). Live convert is skipped unless `DOC_EXTRACT_DOCLING_URL` is set (uses `curl` if present).

```bash
ros -e '(asdf:test-system "doc-extract-backend-docling")' -q
DOC_EXTRACT_DOCLING_URL=http://127.0.0.1:5001 ros -e '(asdf:test-system "doc-extract-backend-docling")' -q
```

Conditions: protocol `doc-extract-error`. Restarts at the convert boundary: `use-value`, `retry` (plus the protocol `:around` on `extract-document` / `extract-text`).

## License

MIT — see [LICENSE](LICENSE).
