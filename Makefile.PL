use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'Biblio::WebPortal',
    'VERSION_FROM'	=> 'WebPortal.pm',
    'PREREQ_PM'		=> {
		DB_File => 1.8,
		Biblio::Thesaurus => 0.16,
			    }, 
);
