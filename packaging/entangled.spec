Name:           entangled
Version:        1.0.3
Release:        1%{?dist}
Summary:        Literate Programming assistant

License:        ASL 2.0
URL:            https://entangled.github.io/
Source0:        https://github.com/entangled/entangled/archive/v%{version}.tar.gz

# BuildRequires:  
# Requires:       

%description
Entangled helps you write Literate Programs in Mardown. You put all your code inside Markdown code blocks. Entangled automatically extracts the code and writes it to more traditional source files. You can then edit these generated files, and the changes are being fed back to the Markdown.

%prep
%autosetup

%build
cabal configure --prefix %{buildroot}
cabal build

%install
rm -rf $RPM_BUILD_ROOT
cabal install


%files
%license LICENSE
%doc README.md



%changelog
* Wed Jun  3 2020 Johan Hidding <j.hidding@esciencecenter.nl>
- 
