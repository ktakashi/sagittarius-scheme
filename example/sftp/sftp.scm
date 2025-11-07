(import (rnrs) (rfc sftp) (pp) (srfi :26))

(call-with-sftp-connection 
  "localhost"    ;; hostname
  "22"           ;; port number
  (lambda (conn)
    ;; read directory
    (pp (sftp-readdir conn "."))
    ;; only short names
    (pp (sftp-readdir-as-filenames conn "."))
    ;; only long names (usually POSIX 'ls -l' format)
    (pp (sftp-readdir-as-longnames conn "."))

    ;; retrieve a file as a bytevector
    (print (utf8->string (sftp-read! conn "reading/file" 
                                    (sftp-binary-receiver))))
    ;; store a file to local file directly
    (sftp-read conn "/a/file/path"
	(sftp-file-receiver "where/to/store" :options (file-options no-fail)))

    ;; upload a file
    (let ((handle (sftp-open conn "boo" 
                    (bitwise-ior +ssh-fxf-creat+ +ssh-fxf-write+))))
     (call-with-input-file "a/local/file" (cut sftp-write! conn handle <>)
                           :transcoder #f))

    ;; rename a file
    (sftp-rename! conn "boo" "foo")
    ;; remove a file
    (sftp-remove! conn "foo")
    ;; create a directory
    (sftp-mkdir! conn "boo")
    ;; remove a directory
    (sftp-rmdir! conn "boo")

    ;; create a symbolic link
    (pp (sftp-symlink! conn "/tmp" "tmp"))
    ;; get a actual path of symbolic link
    (pp (sftp-readlink conn "tmp"))
    ;; get a real path. (usually an absolute path)
    (pp (sftp-realpath conn "../")))
  :authenticate (sftp-keyboard-interactive-authentication "username"))
