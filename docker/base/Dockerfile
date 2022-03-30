FROM twyair/safot-smlnj:latest

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update \
    && apt install -y curl \
    && curl -sL https://deb.nodesource.com/setup_14.x | bash - \
    && apt update \
    && apt install -y nodejs ipython3 jupyter jupyter-notebook git python3-pip

RUN pip3 install jupyter-server jupyterlab
RUN npm install -g npm@latest
RUN npm install -g ijavascript
RUN ijsinstall

EXPOSE 16788 16789
