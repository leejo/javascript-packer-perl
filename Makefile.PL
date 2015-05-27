use strict;
use warnings;

use inc::Module::Install;

all_from        'lib/JavaScript/Packer.pm';
test_requires   'Test::More'        => 0;
requires        'Regexp::RegGrp'    => "1.001_001";

repository 'http://github.com/leejo/javascript-packer-perl';
bugtracker 'http://github.com/leejo/javascript-packer-perl/issues';

clean_files 't/scripts/s*-got.js';

auto_install();
WriteAll();
