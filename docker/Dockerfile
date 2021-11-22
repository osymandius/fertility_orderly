FROM mrcide/naomi-base:latest

RUN install_packages \
        tidyverse \
        purrr

COPY docker/bin /usr/local/bin/

RUN install_remote \
        mrc-ide/naomi \
        vimc/orderly \
        reside-ic/spud \
        vimc/orderly.sharepoint \
        osymandius/dfertility

RUN install_inla