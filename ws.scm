(import (rnrs) (rfc websocket) (util logging))

;; Creates an WebSocket object
(define websocket (make-websocket "wss://echo.websocket.org/"
		   :logger (make-logger +debug-level+ (make-appender "~m"))))
;;(define websocket (make-websocket "ws://localhost:8080/chat"))

;; Sets text message event handler
(websocket-on-text-message websocket
  (lambda (ws text) (display "resp: ") (display text) (newline)))

;; Opens the WebSocket
(websocket-open websocket)

;; Sends a message to endpoint
(websocket-send websocket "Hello")
(websocket-send websocket "world!")

;; Closes the WebSocket
(websocket-close websocket)
