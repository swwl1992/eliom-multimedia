Instructions
============

This project is (initially) generated by eliom-destillery as the basic
project "subtitle".

Generally, you can compile it and run ocsigenserver on it by
  $ make test.byte (or test.opt)
See below for other useful targets for make.

Generated files
---------------

The following files in this directory have been generated by
eliom-destillery:

 - subtitle.eliom
   This is your initial source file.
   All Eliom files (*.eliom, *.eliomi) in this directory are
   automatically considered.  To add a .ml/.mli file to your project,
   append it to the variable SERVER_FILES or CLIENT_FILES.

 - static/
   The content of this folder is statically served. Put your CSS or
   additional JavaScript files here!

 - Makefile.options
   Configure your project here!

 - subtitle.conf.in
   This file is a template for the configuration file for
   ocsigenserver. You will rarely have to edit itself - it takes its
   variables from the Makefile.options. This way, the installation
   rules and the configuration files are synchronized with respect to
   the different folders.

 - Makefile
   This contains all rules necessary to build, test, and run your
   Eliom application. You better don't touch it ;) See below for the
   relevant targets.

 - README
   Not completely describable here.


Makefile targets
----------------

Here's some help on how to work with this basic destillery project:

 - Test your application by compiling it and running ocsigenserver locally
     $ make test.byte (or test.opt)

 - Compile it only
     $ make all (or byte or opt)

 - Deploy your project on your system
     $ sudo make install (or install.byte or install.opt)

 - Run the server on the deployed project
     $ sudo make run.byte (or run.opt)

   If WWWUSER in the Makefile.options is you, you don't need the
   `sudo'. If Eliom isn't installed globally, however, you need to
   re-export some environment variables to make this work:
     $ sudo PATH=$PATH OCAMLPATH=$OCAMLPATH LD_LIBRARY_PATH=$LD_LIBRARY_PATH make run.byte/run.opt

 - If you need a findlib package in your project, add it to the
   variables SERVER_PACKAGES and/or CLIENT_PACKAGES. The configuration
   file will be automatically updated.
