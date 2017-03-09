(in-package #:talos)

(defctype bool :int)
(defctype word :unsigned-short)
(defctype dword :unsigned-long)
(defctype handle :pointer)
(defctype ipaddr :unsigned-long)

(defcstruct c-ip-option-information32
  (ttl :unsigned-char)
  (tos :unsigned-char)
  (flags :unsigned-char)
  (options-size :unsigned-char)
  (data :pointer))

(defcstruct c-icmp-echo-reply32
  (ipaddr ipaddr)
  (status :unsigned-long)
  (roundtrip-time :unsigned-long)
  (data-size :unsigned-short)
  (reserved :unsigned-short)
  (data :pointer)
  (options c-ip-option-information32))

(define-foreign-library iphlpapi
  (:win32 "iphlpapi.dll"))

(use-foreign-library iphlpapi)

(defcfun ("IcmpCreateFile" c-icmp-create-file) handle)
(defcfun ("IcmpCloseHandle" c-icmp-close-handle) bool
  (handle handle))
(defcfun ("IcmpSendEcho" c-icmp-send-echo) dword
  (handle handle)
  (destination-address ipaddr)
  (req-data :pointer)
  (req-size word)
  (req-options :pointer)
  (reply-buffer :pointer)
  (reply-size dword)
  (timeout dword))

(defun ping (hostname &optional (timeout 2500))
  (let* ((handle (c-icmp-create-file))
         (addr (resolve hostname)))
    (with-foreign-string ((data data-size) "ICMP Ping")
      (c-icmp-send-echo handle addr data data-size nil))
    (c-icmp-close-handle handle)))

(defun resolve (hostname)
  #+ccl
  (ccl:lookup-hostname hostname))
