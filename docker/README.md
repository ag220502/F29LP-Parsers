# Docker

This folder contains the Docker file, in case you want to install it manually.

You can install it via: 
```
docker build --pull -f "dockerfile" -t ocamljupyter:2023 "."
```

As when installed via the repository, you can then run the Docker image via: 
```
docker run -it -p 8888:8888 -v "$(pwd):/lectures" ocamljupyter
```

and, once started, run the notebook in the Gitlab directory via: 
```
jupyter notebook --ip=0.0.0.0 
```