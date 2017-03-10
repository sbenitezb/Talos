(in-package #:talos)

(cffi:defctype bool :int)
(cffi:defctype word :unsigned-short)
(cffi:defctype dword :unsigned-long)
(cffi:defctype handle :pointer)
(cffi:defctype ipaddr :unsigned-long)

(cffi:defcstruct c-ip-option-information32
  (ttl :unsigned-char)
  (tos :unsigned-char)
  (flags :unsigned-char)
  (options-size :unsigned-char)
  (data :pointer))

(cffi:defcstruct c-icmp-echo-reply32
  (ipaddr ipaddr)
  (status :unsigned-long)
  (roundtrip-time :unsigned-long)
  (data-size :unsigned-short)
  (reserved :unsigned-short)
  (data :pointer)
  (options (:struct c-ip-option-information32)))

(cffi:define-foreign-library iphlpapi
  (:windows "iphlpapi.dll"))

(cffi:use-foreign-library iphlpapi)

(cffi:defcfun ("IcmpCreateFile" c-icmp-create-file) handle)
(cffi:defcfun ("IcmpCloseHandle" c-icmp-close-handle) bool
  (handle handle))
(cffi:defcfun ("IcmpSendEcho" c-icmp-send-echo) dword
  (handle handle)
  (destination-address ipaddr)
  (req-data :pointer)
  (req-size word)
  (req-options :pointer)
  (reply-buffer :pointer)
  (reply-size dword)
  (timeout dword))

(defun ping (hostname &optional (timeout 1000))
  (declare (ignorable timeout))
  (if-let (addr (resolve hostname))
    (let ((handle (c-icmp-create-file))
          (buffer-size (cffi:foreign-type-size '(:struct c-icmp-echo-reply32)))
          (status :error))
      (cffi:with-foreign-string ((data data-size) "ICMP Ping -- 12345678")
        (cffi:with-foreign-pointer (buffer (+ buffer-size data-size))
          (c-icmp-send-echo handle (ccl::htonl addr) data data-size (cffi:null-pointer)
                            buffer (+ buffer-size data-size) timeout)
          (setf status (translate-icmp-status buffer))))
      (c-icmp-close-handle handle)
      status)
    :unreachable))

(defun translate-icmp-status (buffer)
  (cffi:with-foreign-slots ((status) buffer (:struct c-icmp-echo-reply32))
    (case status
      (0 :success)
      (11002 :unreachable)
      (11003 :unreachable)
      (11005 :blocked)
      (11010 :timeout)
      (otherwise :error))))

(defun resolve (hostname)
  #+ccl
  (ccl:lookup-hostname hostname))
