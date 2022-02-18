while [[ `brew list | wc -l` -ne 0 ]]; do
    for EACH in `brew list`; do
        brew uninstall --force --ignore-dependencies $EACH
    done
done
