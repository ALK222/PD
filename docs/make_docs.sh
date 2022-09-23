#!/bin/bash
haskfiles=""
prolfiles=""
while read -d $'\0' file
  do
    if [ "${file: -3}" == ".hs" ]
    then
        haskfiles+=" ${file}"
    elif [ "${file: -3}" == ".pl" ]
    then
        if [[ "$file" != *"fichas.pl" ]]
        then
            prolfiles+=" ${file}"
        fi
    fi
done < <(find ../src/Labos/ -type f -print0)
haddock -h --title="Prácticas Haskell" -o ./html/haskell $haskfiles -w 
haddock --latex -o ./latex/haskell $haskfiles -w --title="Prácticas Haskell"
swipl -g "doc_server(4000),portray_text(true),use_module(library(doc_files)),doc_save('../src/Labos', [doc_root('./html/prolog'), recursive(true)])" -t halt -s $prolfiles 
