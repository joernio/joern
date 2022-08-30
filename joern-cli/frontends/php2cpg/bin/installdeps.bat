REM Install `composer`
php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"

php composer-setup.php --quiet
rm composer-setup.php

REM Install Nikic's php-parser
php composer.phar require nikic/php-parser:4.13.2
