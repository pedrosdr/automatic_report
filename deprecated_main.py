from PIL import Image
from PyPDF2 import PdfMerger
import os

# convert images to pdf
for root, dirs, files in os.walk('layout/img'):
    for file in files:
        filename = file.replace('.jpg', '')
        filepath = os.path.join(root, file).replace('\\', '/')
        filepath_without_extension = filepath.replace('.jpg', '')
        img = Image.open(filepath).convert('RGB')
        img.save('layout/pdf/' + filename + '.pdf')

# merge the pdf files
merger = PdfMerger(strict=False)

for root, dirs, files in os.walk('layout/pdf'):
    for file in files:
        filepath = os.path.join(root, file).replace('\\', '/')
        merger.append(filepath)

merger.write('relatorio.pdf')