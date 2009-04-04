(when (file-exists-p "~/git-projects/libnifalcon/CMakeLists.txt")
  (setq libnifalcon-ede-project
	(ede-cpp-root-project "libnifalcon"
			      :file "~/git-projects/libnifalcon/CMakeLists.txt"
			      :include-path '(
					      "/include/"
					      "/include/falcon"
					      "/include/falcon/core"
					      "/include/falcon/firmware"
					      )
			      :local-variables '(
						 (compile-command . "cd ~/git-projects/libnifalcon/build_darwin_10.5_x86/; make -j2")
						 )

			      )
	)
  )