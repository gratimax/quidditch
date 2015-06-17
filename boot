node qd.js quidditch-compiler.lisp > qd-boot.js
node qd-boot.js quidditch-compiler.lisp > qd-boot2.js
echo "showing diff"
diff qd-boot.js qd-boot2.js