library(reticulate)

# Install Python 3.12
install_python("3.12")

# Create the virtual environment
virtualenv_create(
  envname = "keras-env",
  python = "3.12"
)

# Install the required packages
virtualenv_install(
  envname = "keras-env",
  packages = c(
    "numpy<2",
    "tensorflow==2.19.0",
    "keras",
    "pydot",
    "scipy",
    "pandas",
    "Pillow",
    "ipython"
  )
)

# Tell reticulate to use this environment
use_virtualenv("keras-env", required = TRUE)

# Check Python configuration
py_config()