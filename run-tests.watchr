watch('(.*\.scm)$') { |md| 
  file = "#{md[0]}"
  system "clear"
  sleep(1.0/3)
  system "csi -b -q -r5rs-syntax -n #{file}" 
}
