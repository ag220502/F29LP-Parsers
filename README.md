# F29LP

## Installation on Your Computer
It is strongly recommended to install the course material via Docker. 

There are three steps:
1. Start the terminal. Ensure to use the PowerShell or Linux Bash Shell on Windows.
    Get the GitLab repo:

    ```
    git clone git@gitlab-student.macs.hw.ac.uk:ks2053/F29LP.git
    ```

    (You might alternatively want to fork the repo: https://repository.prace-ri.eu/git/help/user/project/repository/forking_workflow.md)

    Go into the relevant directory: 

    ```
    cd F29LP
    ```

2. Install Jupyter notebook.

    - Install Docker: https://www.docker.com.

      Ensure that Docker is running. You might have to restart Docker after restarting your computer.

    - Install and start the relevant Docker file: 
      ```
      docker run -it -p 8888:8888 -v "$(pwd):/lectures" kstarkhwu/f29lp:latest
      ```
      
      - If you have a MacBook, you might need to use the arm64 image instead. 
        You can check this via ``uname -m``: if it says "amd64" or "x86_64" use the command above. If it says "arm64", run
        ```
        docker run -it -p 8888:8888 -v "$(pwd):/lectures" kstarkhwu/f29lp:arm64
        ```

    - In there, run: 

      ```
      jupyter notebook --ip=0.0.0.0
      ```

      and then the link appearing starting with ``http://127.0.0.1`` should be openable in a browser.

    - You'll want to make copies of all files you want to work on to avoid merge conflicts later.

## Running the Notebook Subsequent Times

1. Ensure the GitLab repo is in the up-to-date state: 

    ```
    git pull
    ```

2. Re-run the Docker files:

    -  Run

      ```
      docker run -it -p 8888:8888 -v "$(pwd):/lectures" kstarkhwu/f29lp
      ```

    - In there, run: 

      ```
      jupyter notebook --ip=0.0.0.0
      ```

      and then the link appearing starting with ``http://127.0.0.1`` should be openable in a browser.

      

      ## On Lab Computers

The (Linux) lab computers have Jupyter notebooks directly installed. 

You hence only have to:
1. Get the GitLab repo:

   ```
   git clone git@gitlab-student.macs.hw.ac.uk:ks2053/F29LP.git
   ```

   Go into the relevant directory: 

   ```
   cd F29LP
   ```

2. Run: 

   ```
   jupyter notebook 
   ```

   and then the link appearing should be openable in a browser.
    You'll want to make copies of all files you want to work on to avoid merge conflicts later.



## FAQs 

- *I am on Windows, and the following command fails:* 

```
docker run -it -p 8888:8888 -v "$(pwd):/lectures" kstarkhwu/f29lp:latest
```

Use the PowerShell or Linux Bash Shell. The old DOS command prompt does not support the above command. 

- *I have problems with installing Docker on Linux Mint.* One student reported that these two guides were useful: [this](https://computingforgeeks.com/install-docker-docker-compose-on-linux-mint/) guide and [this](https://computingforgeeks.com/how-to-install-latest-docker-compose-on-linux/) guide.

- *Docker starts but I cannot see the lecture/lab notebook.* Ensure you are in the right folder when running Docker/the Jupyter notebook. The ``"$(pwd):/lectures"`` part ensures that you can see the folder content you are currently in.

- *Cloning the GitLab repo fails.* GitLab needs to ensure that you are a student at HWU. Ensure to have registered an SSH key on the machine you are using (see e.g. https://docs.gitlab.com/ee/user/ssh.html) or download and unpack the repo manually.

- *I tried everything and did not get the Jupyter Notebook to run.* Please write to the lab helpers or come to me after the lecture - we will try to find a solution. You should still be able to run the Jupyter notebooks on the Linux lab computers.

- If you find no way to install Docker on your own computer, Ben Agnew describes in his branch how he set up connecting to the lab computers via X2go: 
   https://gitlab-student.macs.hw.ac.uk/ba2005/F29LP/-/tree/x2goSSH location: git@gitlab-student.macs.hw.ac.uk:ba2005/F29LP.git
