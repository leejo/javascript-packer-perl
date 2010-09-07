use strict;
use warnings;

use inc::Module::Install;

all_from        'lib/JavaScript/Packer.pm';
test_requires   'Test::More'        => 0;
requires        'Regexp::RegGrp'    => 0;

repository 'http://github.com/nevesenin/javascript-packer-perl';

auto_install();
WriteAll();
