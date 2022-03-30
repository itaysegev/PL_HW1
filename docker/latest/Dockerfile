FROM twyair/safot-revealjs:base

WORKDIR /home/safot

ENV DEBIAN_FRONTEND=noninteractive

COPY start.sh /home/safot

RUN pip install markupsafe==2.0.1

RUN git clone https://github.com/twyair/simple-ismlnj.git /usr/share/jupyter/kernels/smlnj

RUN git clone https://github.com/OpenUniversity/safot.git && cd safot/reveal.js && npm install --production
