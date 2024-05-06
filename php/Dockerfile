FROM php:8.3.7RC1-zts-bullseye

RUN apt-get update

RUN docker-php-ext-configure pcntl --enable-pcntl \
    && docker-php-ext-install pcntl

WORKDIR /var/www/html

CMD [ "php", "main.php" ]