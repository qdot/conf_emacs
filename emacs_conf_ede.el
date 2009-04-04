(setq geotools-ede-project 
	  (ede-cpp-root-project "Geotools" 
				:file "~/build/geotools/CMakeLists.txt"
				:include-path '(
						"/libgeotools/include/"
						"/libgeotools/include/geotools/core"
						"/libgeotools/include/geotools/processors"
						"/libgeotools/include/geotools/math"
						"/libgeotools/include/geotools/qt"
						"/qt-apps"
						"/batch"
						)
				:system-include-path '(
						       "~/build/libdss/libdss/include"
						       "~/build/libdss/libdssutil/include"
						       "~/build/library/usr_darwin_10.5_x86/include"
						       "~/build/library/usr_darwin_10.5_x86/include/boost"
						       )
				:local-variables '(
						   (compile-command . "cd ~/build/geotools/build_darwin_10.5_x86/; make -j2")
						   )
				
				)
	  )

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
