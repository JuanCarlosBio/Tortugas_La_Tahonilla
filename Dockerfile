# Start from an official Ubuntu image
FROM ubuntu:24.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/miniconda3/bin:$PATH"

# Install dependencies
RUN apt-get update && apt-get install -y \
    wget \
    bzip2 \
    ca-certificates \
    libglib2.0-0 \
    vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Download and install Miniconda
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /root/miniconda.sh && \
    bash /root/miniconda.sh -b -u -p /root/miniconda3 && \
    rm -rf /root/miniconda.sh

# Copy app (which includes conda/envs/publicacion.yaml)
COPY . /app
WORKDIR /app

# Create the environment from the specific path you requested
RUN conda env create -f conda/envs/publicacion.yaml && conda clean -a -y

# Set the shell to use the created environment for subsequent RUN commands
# Esto hace que no necesites "activate" en cada línea
SHELL ["conda", "run", "-n", "inventario_especies_gc", "/bin/bash", "-c"]

# Verify environment
RUN conda info --envs

# Install additional dependencies
RUN pip install --upgrade pip

# Init conda para que la shell interactiva lo reconozca
RUN conda init

# Set default command
CMD ["/bin/bash"]