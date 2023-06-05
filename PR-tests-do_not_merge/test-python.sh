#!/bin/sh

if [ ! -d "python/venv" ]; then
    cd python
    python3 -m venv venv
    source venv/bin/activate
    pip install -r requirements.txt
    deactivate
    cd ..
fi

emacs -Q --batch -l init.el
