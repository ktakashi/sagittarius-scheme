(import (rnrs)
	(rnrs eval)
	(win32 gui)
	(win32 user)
	(win32 kernel)
	(win32 shell) ;; TODO remove
	(win32 common-control)
	(sagittarius object)
	(sagittarius ffi)
	(srfi :13)
	(srfi :18)
	(srfi :39 parameters)
	(scheme load)
	(util file)
	(clos user))

(define (load-in-thread dir file output env)
  (thread-start!
   (make-thread
    (lambda ()
      (parameterize ((current-output-port output)
		     (current-error-port output)
		     (current-directory dir))
	(guard (e (else (report-error e)))
	  (load file env)))))))

(define menu-bar (make <win32-menu-bar>))
(define file-menu  (make-win32-menu <win32-menu> :name "&File"))
(define open-menu-item (make <win32-menu-item> :name "New &Tab"
			     :hmenu (integer->pointer 10)))
(define quit-menu-item (make <win32-menu-item> :name "&Quit"
			     :hmenu (integer->pointer 20)))

(define console-menu  (make-win32-menu <win32-menu> :name "&Console"))
(define toggle-menu-item (make <win32-menu-item> :name "&Show"
			       :hmenu (integer->pointer 110)))

(define file-select (make-win32-file-select
		     :extension "scm"
		     :filters '("Scheme File(*.scm)" "*.scm")
		     :title "Choose Scheme file to run"))
(define directory-select (make-win32-directory-select
			  :title "Choose working directory"))
(win32-set-event-handler! open-menu-item 'click
 (lambda (me message wparam lparam)
   (let* ((name (format "Scheme (~d)" (+ (length (~ tab-container 'tabs)) 1)))
	  (tab (setup-tab name)))
     (win32-add-component! tab-container tab)
     ;; oh this sucks...
     (win32-show tab))))

(win32-set-event-handler! quit-menu-item 'click
 (lambda (me message wparam lparam)
   (post-quit-message 0)))

(win32-set-event-handler! toggle-menu-item 'click
 (lambda (me message wparam lparam)
   (define console-hwnd (get-console-window))
   (cond ((is-window-visible console-hwnd)
	  (show-window console-hwnd SW_HIDE)
	  (win32-menu-set-text! toggle-menu-item "&Show"))
	 (else
	  (show-window console-hwnd SW_SHOW)
	  (win32-menu-set-text! toggle-menu-item "&Hide")))))

(win32-add-menu-item! file-menu open-menu-item)
(win32-add-menu-item! file-menu quit-menu-item)
(win32-add-menu! menu-bar file-menu)

;; a bit of adhoc solution...
(unless (getenv "PROMPT")
  (show-window (get-console-window) SW_HIDE)
  (win32-add-menu-item! console-menu toggle-menu-item)
  (win32-add-menu! menu-bar console-menu))


(define-class <auto-resizable-tab-conainer> (<win32-tab-container> <win32-auto-resize>) ())
(define tab-container (make <auto-resizable-tab-conainer>))

(define-class <auto-resizable-tab-panel>
  (<win32-closable-tab-panel> <win32-auto-resize>) ())
(define-class <auto-resizable-text-area> (<win32-multi-text-edit> <win32-auto-resize>) ())

;; (define cross-icon (load-system-icon IDI_APPLICATION))
;; (define image-list (make <win32-image-list>
;; 		     :width 30 :height 30
;; 		     :images (list cross-icon)))
;; (win32-tab-container-set-image-list! tab-container image-list)

(define close-icon-hbitmap
  (load-image null-pointer OBM_BTSIZE IMAGE_BITMAP 0 0 0))
(define close-icon (make <win32-bitmap-image> :hbitmap close-icon-hbitmap))

(define (->output-port edit)
  (define (write! s start len)
    (win32-edit-append-text! edit (string-copy s start (+ start len)))
    len)
  (make-custom-textual-output-port "multiline-text-edit-output-port"
   write! #f #f #f))

(define (setup-tab name)
  (let ((tab (make <auto-resizable-tab-panel> :name name
		   ;; :close-button close-icon
		   :destroy-children? #t))
	(label (make <win32-label> :name "File"
		     :x 0 :y 0 :width 150 :height 30 :window-style WS_EX_RIGHT))
	(edit (make <win32-text-edit> :value ""
		    :x 150 :y 0 :width 200 :height 30 :style WS_BORDER))
	(open (make <win32-button> :name "Open file"
		    :x 350 :y 0 :width 100 :height 30))
	(load (make <win32-button> :name "Load"
		    :x 450 :y 0 :width 100 :height 30))

	(wlabel (make <win32-label> :name "Working directory"
		     :x 0 :y 30 :width 150 :height 30 :window-style WS_EX_RIGHT))
	(wedit (make <win32-text-edit> :value ""
		     :x 150 :y 30 :width 200 :height 30 :style WS_BORDER))
	(wopen (make <win32-button> :name "Open directory"
		    :x 350 :y 30 :width 100 :height 30))
	
	(label2 (make <win32-label> :name "Output"
		      :x 0 :y 60 :width 100 :height 30))
	(output (make <win32-multi-text-edit> :value ""
		      :x 0 :y 90 :width 700 :height 600
		      :style WS_BORDER)))
    (win32-add-component! tab label)
    (win32-add-component! tab edit)
    (win32-add-component! tab open)
    (win32-add-component! tab load)

    (win32-add-component! tab wlabel)
    (win32-add-component! tab wedit)
    (win32-add-component! tab wopen)
    
    (win32-add-component! tab label2)
    (win32-add-component! tab output)
    (win32-set-event-handler! open 'click
     (lambda (b message wparam lparam)
       (let ((file (win32-open-file-select file-select window)))
	 (when file
	   ;; set file to edit
	   (win32-edit-set-text! edit file)
	   (let-values (((dir file ext) (decompose-path file)))
	     (win32-edit-set-text! wedit dir))))))
    (win32-set-event-handler! load 'click
     (lambda (b message wparam lparam)
       (let ((file (win32-edit-get-text edit))
	     (dir (win32-edit-get-text wedit)))
	 (when (file-exists? file)
	   (unless (string=? (~ tab 'tab-name) file)
	     (win32-tab-panel-set-tab-name! tab file))
	   (let ((oport (->output-port output)))
	     (guard (e (else (report-error e)))
	         ;; keep the same as default ;)
	       (define env (environment '(core) '(core base)
					'(sagittarius)
					'(sagittarius fixnums)
					'(sagittarius flonums)))
	       (load-in-thread dir file oport env)))))))

    (win32-set-event-handler! wopen 'click
     (lambda (b message wparam lparam)
       (let ((path (win32-open-directory-select directory-select window)))
	 (when path (win32-edit-set-text! wedit path)))))
    tab))

(define initial-tab (setup-tab "Scheme (1)"))
(win32-add-component! tab-container initial-tab)

(define window (make <win32-window> :name "Scheme File Loader"))
(win32-add-component! window menu-bar)
(win32-add-component! window tab-container)

(win32-set-size! window 750 810)

(window)
