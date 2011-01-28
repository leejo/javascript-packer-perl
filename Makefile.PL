use strict;
use warnings;

use inc::Module::Install;

all_from        'lib/JavaScript/Packer.pm';
test_requires   'Test::More'        => 0;
requires        'Regexp::RegGrp'    => "1.000";

repository 'http://github.com/nevesenin/javascript-packer-perl';

clean_files 't/scripts/s*-got.js';

auto_install();
WriteAll();
