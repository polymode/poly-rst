-*- polymode-default-inner-mode: python-mode -*-

ReSTructued Text Example
========================

This is a document that uses various programming languages in order to test the
functionality of *poly-mode* in *Emacs*.

Generic Code Block
------------------

This is a generic code-block. It should not receive special highlights. 

::

    import sys

    class python_test(object):
        pass

    def main():
        pass

    if __name__ == '__main__':
        sys.exit(main())


This is a generic code-block without language specifiers. It should be
highlighted with the value of `polymode-default-inner-mode` if non-nil.

.. code-block::

    import sys

    class python_test(object):
        pass

    def main():
        pass

    if __name__ == '__main__':
        sys.exit(main())


Python
------

.. sourcecode:: python
                
    import sys

    class python_test(object):
        pass
    

    def main(ab):
        """This does wierd things.
        
        .. Keyword Arguments:
        :param ab: Test.

        .. Returns:
        :return: The number 42.

        """
        return 42


    if __name__ == '__main__':
        sys.exit(main())

.. warning::

   The following code works on Python 2, but not Python 3:

   .. code-block:: python
       :caption: Python Warning Test

       print "Hello world"
       assert 5 / 2 == 2

   In Python 3, you need to add parentheses around ``"Hello world"``, and the division call
   will return 2.5 instead of 2.


C/C++
-----

.. In tests C mode interacts in a peculiar way with python's dockstring from
   above. In interactive works it works fine. Disabling for now.

.. highlight:: d++
      :caption: C/C++

      #include <iostream>

      using namespace std;

      class test { public: test() {} ~test() {} }

        int main(char argc, char *argv[])
        {
          (void) argc;
          (void) argv;
          cout << "Hello, World!" << endl;
          return 0;
        }


.. code:: d

    #include <stdio.h>
	
	struct test
    {
	  int dummy;
    }

    int main(char argc, char *argv[])
    {
	  (void) argc;
	  (void) argv;
	  printf("Hello, world!\n");
	  return 0;
    }


Rust
----

How does it handle relatively new languages?

.. code-block:: rust
   :caption: Rust

    use std::fmt;

    fn main()
    {
        println!("Hello, world!");
    }


LaTeX
-----

.. code-block:: LaTeX
   :caption: Latex

    \documentclass{beamer}
    \usepackage{etex}
    \usepackage[latin1]{inputenc}
    \usepackage{mathtools}

    \title[]{Video Modeling}
    \subtitle{}
    \author{Gustaf Waldemarson}
    \institute{ARM Sweden AB}
    \date{\today}
    \subject{Computer Science}

    \begin{document}

    \end{document}


Make
----

.. code:: makefile

    CPPFLAGS= -D_POSIX_C_SOURCE=200809L
	CFLAGS=-g3 -std=c99
	LDLIBS=-lxcb
    PROGS= xcb_query_keymap xcb_events xcb_modmap xcb_keyboard_grab

    .PHONY: all
    all: $(PROGS)

    .PHONY: clean
    clean:
        $(RM) $(PROGS)


Lisp
----

.. code:: lisp

    (defparameter *small* 1)
    (defparameter *big* 100)

    (defun guess-my-number ()
      (ash (+ *small* *big*) -1))


Emacs Lisp
----------

If it handled lisp okay, how does it handle the Emacs Lisp dialect? Or
other dialects for that matter?

.. code:: elisp

    (defun srt-renumber-file ()
      "Re-number all lines in the current subrip subtitle file."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((cnt 0))
          (while (search-forward-regexp "^[0-9]+$")
            (replace-match (number-to-string (cl-incf cnt)))))))

