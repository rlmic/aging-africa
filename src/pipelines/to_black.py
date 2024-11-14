import cv2

# Read the image
image = cv2.imread('/Users/danielapintoveizaga/CEGA Dropbox/Daniela Pinto Veizaga/aging-africa/notebooks/figures_paper_jpemi_files/figure-latex/out_agg_ctr_wgt_gen-1.png')

# Convert the image to grayscale (black and white)
gray_image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

# Save or display the black and white image
cv2.imwrite('./bw_image.jpg', gray_image)
cv2.imshow('Black and White Image', gray_image)
cv2.waitKey(0)
cv2.destroyAllWindows()

