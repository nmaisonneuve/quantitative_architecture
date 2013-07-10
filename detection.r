
records = Dir.glob("**/*")
# => ["file1.txt", "file2.txt", "dir1", "dir1/file1.txt", ...]

ls -d ~/work/data/detections/cutout_images | grep "^d" | tr -s ' ' ' ' | cut -d' ' -f9
