(cons*
 (channel
  (name 'dinos)
  (url "https://gitlab.ody5.de/ody55eus/dinos.git")
  (branch "main")
  (introduction
   (make-channel-introduction
    "e802e65a76e49435ded5ef909120b2b84f2e262b"
    (openpgp-fingerprint
     "885B 941B 8221 3321 6D96 0E4C DE2A D6CF 2474 B880"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  ;; (commit "d54973e47b89fe5772a5b6e2d0c0b86acb089e27")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 %default-channels)
