requires 'At', '0.19';
requires 'HTTP::Tiny';
requires 'Mozilla::CA';
requires 'Path::Tiny';
requires 'URI';
requires 'perl', 'v5.40.0';
recommends 'Mojo::UserAgent';

on configure => sub {
    requires 'CPAN::Meta';
    requires 'Exporter', '5.57';
    requires 'ExtUtils::Helpers', '0.028';
    requires 'ExtUtils::Install';
    requires 'ExtUtils::InstallPaths', '0.002';
    requires 'File::Basename';
    requires 'File::Find';
    requires 'File::Path';
    requires 'File::Spec::Functions';
    requires 'Getopt::Long', '2.36';
    requires 'JSON::PP', '2';
    requires 'Path::Tiny';
    requires 'perl', 'v5.40.0';
};

on test => sub {
    requires 'Test2::V0';
};

on develop => sub {
    requires 'CPAN::Uploader';
    requires 'Minilla';
    requires 'Pod::Markdown::Github';
    requires 'Software::License::Artistic_2_0';
    requires 'Test::CPAN::Meta';
    requires 'Test::MinimumVersion::Fast', '0.04';
    requires 'Test::PAUSE::Permissions', '0.07';
    requires 'Test::Pod', '1.41';
    requires 'Test::Spellunker', 'v0.2.7';
    requires 'Version::Next';
    recommends 'Code::TidyAll';
    recommends 'Code::TidyAll::Plugin::PodTidy';
    recommends 'Data::Dump';
    recommends 'Perl::Tidy';
    recommends 'Pod::Tidy';
    recommends 'Test::CPAN::Meta';
    recommends 'Test::MinimumVersion::Fast';
    recommends 'Test::PAUSE::Permissions';
    recommends 'Test::Pod';
    recommends 'Test::Spellunker';
};
