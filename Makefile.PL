use inc::Module::Install;
name 'Lingua-KO-Numbers';
all_from 'lib/Lingua/KO/Numbers.pm';

build_requires 'Test::More';
use_test_base;
auto_include;
WriteAll;
