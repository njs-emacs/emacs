(defun moo ()
  (setq n (open-network-stream "boop" (current-buffer) "boo" 5007 ':type 'plain ':nowait `(:nowait t)))
  (sit-for 0.1)
  (dotimes (i 10)
    (message "%d %s" i (process-status n))
    (sit-for 1.0)
    )
  )

(defun moo ()
;  (setq n (open-network-stream "boop" (current-buffer) "boo" 5007 ':nowait `t))
  (setq n (open-network-stream "boop" (current-buffer) "192.168.1.244" 5007 ':nowait `t))
  (sit-for 0.1)
  (dotimes (i 10)
    (message "%d %s" i (process-status n))
    (sit-for 1.0)
    )
  )

;(moo)


network-stream-use-client-certificatesHTTP/1.0 400 Bad Request
Server: httpd
Date: Fri, 30 Apr 2021 15:04:35 GMT
Content-Type: text/html
Set-Cookie: urn=4e3c358435c93e39; Path=/; HttpOnly
CONTENT-LANGUAGE: en
X-Frame-Options: DENY
Connection: close

<HTML><HEAD><TITLE>400 Bad Request</TITLE></HEAD>
<BODY BGCOLOR="#cc9999"><H4>400 Bad Request</H4>
No request found.
</BODY></HTML>



Process boop<5> connection broken by remote peer

(dns-lookup-host "boo")
dns-lookup-program
(apropos "resolv")
(apropos "host")

Process boop connection broken by remote peer
