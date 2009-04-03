(setq geotools-ede-project 
	  (ede-cpp-root-project "Geotools" :file "~/build/geotools/CMakeLists.txt"
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
												   "/home/qdot/build/libdss/libdss/include"
												   "/home/qdot/build/libdss/libdssutil/include"
												   "/home/qdot/build/library/usr_darwin_10.5_x86/include"
												   "/home/qdot/build/library/usr_darwin_10.5_x86/include/boost"
												   )
                            :local-variables '(
                                               (compile-command . "cd ~/build/geotools/build_darwin_10.5_x86/; make -j2")
                                               )

							)
)