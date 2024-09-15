# Rproject

To load app on docker use command:


docker pull tkaradimos/rproject:latest

And create a container in the app with port number 8180

or download the repository files and run the command in the directory:

docker build -t yourname .

and use this command after you finish building the image: 

docker run -p 8180:8180 yourcontainername

