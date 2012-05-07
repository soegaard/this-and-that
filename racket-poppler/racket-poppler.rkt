#lang racket
(require racket/draw/unsafe/brush
         racket/draw/unsafe/cairo-lib
         ffi/unsafe
         ffi/unsafe/define
         racket/draw/unsafe/cairo
         racket/snip
         racket/gui/base)

;;;
;;; This file contains bindings for the pdf rendering library Poppler.
;;; The documentation for Popper can be found here:
;;;     http://people.freedesktop.org/~ajohnson/docs/poppler-glib/
;;;

;;; This file simply contains an example, that works on my machine.
;;; Modify the code, so it looks for the libraries libgtk-x11-2.0 and libpoppler-glib
;;; in the right places.

;;; Also to get the example at the bottom working, change the path of bla.pdf.

(define-ffi-definer define-glib (ffi-lib "/opt/local/lib/libgtk-x11-2.0"))
(define-glib gtk_init (_fun _pointer _pointer -> _void))
(gtk_init #f #f)

(define-ffi-definer define-poppler (ffi-lib "libpoppler-glib"))

; Structures used in Poppler
(define _PopplerDocumentPointer (_cpointer 'PopplerDocument))
(define _PopplerPagePointer     (_cpointer 'PopplerPage))
(define _PopplerRectangle       (_cpointer 'PopplerRectangle))
(define _PopplerPageTransition  (_cpointer 'PopplerPageTransition))
(define _PopplerLinkMapping     (_cpointer 'PopplerLinkMapping))
(define _PopplerImageMapping    (_cpointer 'PopplerImageMapping))


; Enumerations
(define _PopplerPageLayout      (_enum '(POPPLER_PAGE_LAYOUT_UNSET
                                         POPPLER_PAGE_LAYOUT_SINGLE_PAGE
                                         POPPLER_PAGE_LAYOUT_ONE_COLUMN
                                         POPPLER_PAGE_LAYOUT_TWO_COLUMN_LEFT
                                         POPPLER_PAGE_LAYOUT_TWO_COLUMN_RIGHT
                                         POPPLER_PAGE_LAYOUT_TWO_PAGE_LEFT
                                         POPPLER_PAGE_LAYOUT_TWO_PAGE_RIGHT)))

(define _PopplerPageMode        (_enum '(POPPLER_PAGE_MODE_UNSET
                                         POPPLER_PAGE_MODE_NONE
                                         POPPLER_PAGE_MODE_USE_OUTLINES
                                         POPPLER_PAGE_MODE_USE_THUMBS
                                         POPPLER_PAGE_MODE_FULL_SCREEN
                                         POPPLER_PAGE_MODE_USE_OC
                                         POPPLER_PAGE_MODE_USE_ATTACHMENTS)))

(define-poppler poppler_document_new_from_file (_fun _string _string _pointer -> (_or-null _PopplerDocumentPointer)))
;;; PopplerDocument *poppler_document_new_from_file(const char *uri, const char *password, GError **error);
; Creates a new PopplerDocument. If NULL is returned, then error will be set. 
; Possible errors include those in the POPPLER_ERROR and G_FILE_ERROR domains.
;    uri:      uri of the file to load
;    password: password to unlock the file with, or NULL. [allow-none]
;    error:    Return location for an error, or NULL. [allow-none]
; Returns:     A newly created PopplerDocument, or NULL

(define-poppler poppler_document_new_from_data (_fun _string _int _pointer _pointer -> (_or-null _PopplerDocumentPointer)))
;;; PopplerDocument *poppler_document_new_from_data (char *data,  int length,  const char *password, GError **error);

(define _time_t _long)

; get various properties of the document
(define-poppler poppler_document_get_title         (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_get_author        (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_get_subject       (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_get_keywords      (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_get_creator       (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_get_producer      (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_get_creation_date (_fun _PopplerDocumentPointer -> _time_t))
(define-poppler poppler_document_get_modification_date (_fun _PopplerDocumentPointer -> _time_t))
(define-poppler poppler_document_get_page_layout   (_fun _PopplerDocumentPointer -> _PopplerPageLayout))
(define-poppler poppler_document_get_metadata      (_fun _PopplerDocumentPointer -> _string))
(define-poppler poppler_document_is_linearized     (_fun _PopplerDocumentPointer -> _bool))
(define-poppler poppler_document_get_n_pages       (_fun _PopplerDocumentPointer -> _int)) ; number of pages


(define-poppler poppler_page_get_size     (_fun _PopplerPagePointer (_cpointer _double) (_cpointer _double) -> _void))
; TODO: ...

(define-poppler poppler_document_get_page (_fun _PopplerDocumentPointer _int -> _PopplerPagePointer))
;;; PopplerPage * poppler_document_get_page(PopplerDocument *document, int index);
; Returns the PopplerPage indexed at index. This object is owned by the caller.
;    document:  A PopplerDocument
;    index:     a page index
; Returns :     (transfer full) : The PopplerPage at index

(define-poppler poppler_page_render (_fun _PopplerPagePointer _cairo_t -> _void))
;;; void poppler_page_render (PopplerPage *page, cairo_t *cairo);
; Render the page to the given cairo context. 
; This function is for rendering a page that will be displayed. 
; If you want to render a page that will be printed use poppler_page_render_for_printing() instead
;    page:  the page to render from
;    cairo: cairo context to render to

(define (poppler-document->_cairo_surface_t document page-number width height)
  (let* ([page (poppler_document_get_page document page-number)]
         [surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 width height)]
         [context  (cairo_create surface)])
    (poppler_page_render page context)
    surface))

(define (pdf-file->_cairo_surface_t filename page-number width height)
  (let ([document (poppler_document_new_from_file (string-append "file:" filename) #f #f)])
    (poppler-document->_cairo_surface_t document page-number width height)))
         

(define (pdf-stream->_cairo_surface_t in page-number width height)
  (let* ([bstr (read-bytes in)]
         [len (bytes-length bstr)]
         [document (poppler_document_new_from_data bstr len #f #f)])
    (poppler-document->_cairo_surface_t document page-number width height)))


;;;
;;; EXAMPLE
;;;


(define WIDTH  595)
(define HEIGHT 791)

(let* ([bitmap    (make-bitmap WIDTH HEIGHT)]  ; works
       [filename  "/Users/soegaard/Dropbox/GitHub/this-and-that/racket-poppler/bla.pdf"]
       [document  (poppler_document_new_from_file (string-append "file:" filename) #f #f)]
       [page      (poppler_document_get_page document 0)]
       [surface   (send bitmap get-handle)]
       [context   (cairo_create surface)])
  (cairo_scale context 2.0 2.0)  
  (poppler_page_render page context)
  (make-object image-snip% bitmap)
  #;(let ([x 0.0] [y 0.0])
      (poppler_page_get_size page x y)
      (list x y)))
