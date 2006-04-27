%define ghc_version 6.2

Summary: Haskell utilities for processing XML
Name: HaXml
Version: 1.12
Release: 1
License: GPL/LGPL
Group: Development/Languages/Haskell
URL: http://www.haskell.org/HaXml/
Source0: %{name}-%{version}.tar.gz
NoSource: 0
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot

%description
HaXml is a collection of utilities for parsing, filtering,
transforming, and generating XML documents using Haskell.
Its basic facilities include:

    * a parser for XML,
    * a separate error-correcting parser for HTML,
    * an XML validator,
    * pretty-printers for XML and HTML, 
    * a combinator library for XML transformation,
    * a translator from DTD to Haskell datatypes.


%package ghc%{ghc_version}
Summary: Haskell library for processing XML
Group: Development/Languages/Haskell
Requires: ghc = %{ghc_version}
BuildRequires: ghc = %{ghc_version}

%description ghc%{ghc_version}
HaXml is a collection of utilities for using XML from Haskell.

This package contains the library built for ghc-%{ghc-version}.

%package doc
Summary: Haskell utilities for processing XML documentation
Group: Development/Languages/Haskell

%description doc
HaXml is a collection of utilities for using XML from Haskell.

This package contains the documentation.

%define ghclibdir %{_libdir}/ghc-%{ghc_version}

%prep
%setup -q

%build
./configure --buildwith=ghc-%{ghc_version} --prefix=%{buildroot}%{_bindir}
make

%install
rm -rf $RPM_BUILD_ROOT
perl -pi -e "s|^%{_prefix}|%{buildroot}%{_prefix}|" obj/ghc/{ghcincdir,ghclibdir}
mkdirhier %{buildroot}%{_bindir}
mkdirhier %{buildroot}%{ghclibdir}/imports/HaXml
make install-filesonly
cp -p obj/ghc/pkg.conf %{buildroot}%{ghclibdir}/imports/HaXml/package.conf

%clean
rm -rf $RPM_BUILD_ROOT

%post ghc%{ghc_version}
ghc-pkg-%{ghc_version} --update-package < %{ghclibdir}/imports/HaXml/package.conf

%preun ghc%{ghc_version}
ghc-pkg-%{ghc_version} --remove-package=%{name}

%files
%defattr(-,root,root,-)
%{_bindir}/*
%doc COPYRIGHT LICENCE-GPL LICENCE-LGPL README

%files ghc%{ghc_version}
%defattr(-,root,root,-)
%{ghclibdir}
%doc COPYRIGHT LICENCE-GPL LICENCE-LGPL README examples

%files doc
%defattr(-,root,root,-)
%doc docs

%changelog

* Tue Mar 16 2004 Malcolm Wallace <malcolm@cs.york.ac.uk> 1.12-1
- Updated to HaXml 1.12
- Licence has changed to GPL + LPGL

* Mon Mar 15 2004 Bjorn Bringert <bjorn@bringert.net> 1.11-1
- Updated to HaXml 1.11
- Removed newline that broke postinstall script

* Mon Dec 22 2003 Bjorn Bringert <bjorn@bringert.net> 1.09-5
- Updated to GHC 6.2

* Mon Dec  8 2003 Bjorn Bringert <bjorn@bringert.net> 1.09-4
- Updated to GHC 6.0.1
- Removed newline that broke install rule in spec

* Tue Jun 10 2003 Jens Petersen <petersen@haskell.org> - 1.09-3
- fix post script package location and preun script package name
- no need to make the ghci object file

* Tue Jun 10 2003 Jens Petersen <petersen@haskell.org> - 1.09-2
- add doc files
- include examples in lib package
- add doc package

* Tue Jun 10 2003 Jens Petersen <petersen@haskell.org> - 1.09-1
- Initial packaging.


