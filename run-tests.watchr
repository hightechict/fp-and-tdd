system "clear"
watch('(.*\.scm)$') { |md| 
  file = "#{md[0]}"
  system "clear"
  sleep(1.0/3)
  print file
  system "csi -b -q -r5rs-syntax -w #{file}" 
}
