# Author: Shravan Sanjay Adraker
# Topic: Brain Tumor Detection using openCV, tkinter, numpy, PIL(Python Imaging Library).

#-----------------------------------------------------------------------------------------------------------------------------------

# Importing the necessary libraries and their specific functions.
import cv2
import numpy as np
from tkinter import Tk, Label, Button, filedialog
from PIL import Image, ImageTk

#-----------------------------------------------------------------------------------------------------------------------------------

# OpenCV uses the BGR color format by default, while PIL (Python Imaging Library) and many other libraries use the RGB format.
def display_image(image):
    rgb_image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)  # Converts the image from BGR format to RGB format.
    pil_image = Image.fromarray(rgb_image)  # Converts the NumPy array to a PIL Image object.
    imgtk = ImageTk.PhotoImage(image=pil_image)  # Converts the PIL Image object to a tkinter-compatible PhotoImage object.
    panel.imgtk = imgtk  # Stores a reference to the PhotoImage object.
    panel.config(image=imgtk)  # Updates the Label widget to display the PhotoImage object.
    panel.image = imgtk  # Ensures that the image is kept in memory.

#-----------------------------------------------------------------------------------------------------------------------------------

def detect_tumor(image_path):
    # Load the grayscale image
    image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)  # Loads the image from the specified file path in grayscale mode.
    if image is None:
        return None, "Error loading image"

    # Apply Gaussian blur to the image
    blurred = cv2.GaussianBlur(image, (11, 11), 0)  # Applies a Gaussian blur to the grayscale image.

    # Apply binary thresholding
    _, thresh = cv2.threshold(blurred, 150, 255, cv2.THRESH_BINARY)  # Applies binary thresholding to the blurred image.

    # Find contours in the thresholded image
    contours, _ = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)  # Finds the contours in the thresholded image.

    # Filter out small contours
    min_area = 1000  # Minimum area to be considered as tumor.
    tumor_contours = [c for c in contours if cv2.contourArea(c) > min_area]  # Filters the contours based on their area.

    # If no contours are detected, return "No tumor detected"
    if not tumor_contours:
        return image, "No tumor detected"

    # Draw contours on the original image
    result_image = cv2.cvtColor(image, cv2.COLOR_GRAY2BGR)  # Converts the original grayscale image to BGR color format.
    cv2.drawContours(result_image, tumor_contours, -1, (0, 255, 0), 2)  # Draws the detected contours on the image.

    return result_image, "Tumor found"

#-----------------------------------------------------------------------------------------------------------------------------------

def upload_image():
    file_path = filedialog.askopenfilename()
    if file_path:
        processed_image, message = detect_tumor(file_path)

        result_label.config(text=message)  # Update the result label with the message.
        
        if processed_image is not None:  # If tumor is detected, display the processed image.
            display_image(processed_image)

#-----------------------------------------------------------------------------------------------------------------------------------

# The creation of GUI window and what will come on top(title) of GUI
root = Tk()
root.title("Brain Tumor Detection")
root.configure(bg='#add8e6')  # Light blue background color

# Project details label
project_details = Label(root, text="Author: Shravan Adraker\nContact: 9175825022\n---------------------------------", bg='#add8e6', fg='red')
project_details.pack(pady=10)
project_details = Label(root, text="Brain Tumor Detection Project\nUsing OpenCV and Tkinter", bg='#add8e6', fg='black', font=('Arial', 12, 'bold'))
project_details.pack(pady=10)

# The widget part will come here:
# For Text inside the GUI
panel = Label(root, text="Brain Tumor Image:")
panel.pack(padx=10, pady=10)  # Padding around Label.

# Result message label
result_label = Label(root, text="", bg='#add8e6', font=('Arial', 12))
result_label.pack(pady=10)

# For Button inside the GUI
upload_button = Button(root, text="Upload Image", command=upload_image)
upload_button.pack(pady=10)

# To launch the GUI
root.mainloop()

