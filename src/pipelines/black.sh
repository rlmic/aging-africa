#!/bin/bash

# Directory containing the images
IMAGE_DIR="/Users/danielapintoveizaga/CEGA Dropbox/Daniela Pinto Veizaga/aging-africa/notebooks/figures_paper_jpemi_files/figure-latex"

# Loop through each .png file in the directory
for IMAGE_PATH in "$IMAGE_DIR"/*.png; do
    

    # Extract the filename without extension
    BASENAME=$(basename "$IMAGE_PATH" .png)
    
    # Define the output black and white PNG file path
    OUTPUT_PNG="$IMAGE_DIR/${BASENAME}-bw.png"

    # Convert the image to black and white and save as PNG
    convert "$IMAGE_PATH" -colorspace Gray "$OUTPUT_PNG"

    echo "Converted $IMAGE_PATH to $OUTPUT_PNG"

done

echo "All images converted to BLACK AND WHITE."

