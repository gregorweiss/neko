#!/usr/bin/bash

# ============================================================================ #
# Define the help function

function help() {
    echo -e "run.sh [Nx] [Ny] [Nz]"
    echo -e "  Generate a mesh and run the immersed_bunny case."
    echo -e "  The input arguments are the number of cells in the x, y, and z"
    echo -e "  directions, respectively."
    echo -e ""
    echo -e "  If no input arguments are provided, the default mesh size is"
    echo -e "  16x8x8."
    echo -e ""
    echo -e "  See Readme for additional details."
    exit 0
}

# Search for "-h" or "--help" in the input arguments
for arg in $@; do
    if [[ $arg == "-h" ]] || [[ $arg == "--help" ]]; then
        help
    fi
done

# ============================================================================ #
# Ensure Neko can be found and set default mesh size

if [ "$NEKO_DIR" ]; then
    export PATH=$NEKO_DIR:$PATH
fi

if [[ -z $(which neko) ]]; then
    echo -e "Neko not found." >&2
    echo -e "Please ensure Neko is installed and in your PATH." >&2
    echo -e "Alternatively, set the NEKO_DIR environment variable." >&2
    exit 1
fi

if [ $# == 0 ]; then
    Nx=16 && Ny=8 && Nz=8
elif [ $# == 3 ]; then
    Nx=$1 && Ny=$2 && Nz=$3
else
    echo -e "Invalid number of input arguments." >&2
    help
fi

# ============================================================================ #
# Generate mesh and run case

echo "Generating mesh with dimensions: $Nx $Ny $Nz"
genmeshbox 0 4 0 1 0 1 $Nx $Ny $Nz .false. .false. .false.
neko immersed_bunny.case

# End of file
# ============================================================================ #
