doc <:doc< -*- Mode: text -*-
   @spelling{typeclass typesystem typeclasses}

   @begin[doc]
   @chapter[declare]{Declarations}
   @end[doc]

   @begin[license]
   Copyright (C) 2005 Mojave Group, Caltech

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Jason Hickey
   @email{jyh@cs.caltech.edu}
   @end[license]
   @docoff
>>
extends Comment
extends Summary

doc <:doc<
@begin[doc]

In order for a term to be used in @MetaPRL, it must be declared first.  @MetaPRL uses a simple
typesystem to classify these terms, where there are three kinds of declarations: typeclasses, types,
and normal terms.  Types are used to classify terms.  Typeclasses are like types, but they are
simple (they have no parameters or subterms), and they allow subtyping.

Before describing the kinds of declarations, it is worth clarifying one point.  Types (and
typeclasses) are denoted by terms.  @emph{Every} term has a type, even the terms that denote types
and typeclasses.  The type of a term that represents a type need not be related.

There are several builtin typeclasses, defined in the @hrefmodule[Perv] module.

@begin[itemize]

@item{{@code{Term} is the default typeclass for normal terms.}}

@item{{@code{Ty} is the default typeclass for terms that represent types and typeclasses.}}

@item{{@code{Dform} is the typeclass used for display form terms that have no logical meaning.
@code{Dform} is a superclass of @code{Term}.}}

@end[itemize]

@section[syntax]{Syntax}

@subsubsection[typeclasses]{Typeclasses}

A term is declared with the @code{declare} keyword.  A typeclasses is declared in one of the
following forms.

@begin[verbatim]
declare typeclass opname [: type]
declare typeclass opname [: type] -> super_opname
declare typeclass opname [: type] <- sub_opname
@end[verbatim]

These forms declare a new typeclass named with the given @code{opname}.  The @code{[: type]} is
optional, and specifies the type of the term representing the typeclass.  The @code{-> super_opname}
specifies that the @code{opname} typeclass is a subclass of @code{super_opname}.
The @code{<- sub_opname} specifies that @code{sub_opname} is a subclass of @code{opname}.

In the following example, we define two typeclasses $A$ and $B$, where $A$ is a subtype of
@code{Term}, and $B$ is a subtype of $A$.  This means that any term $t$ that has type $t : B$ can be
used anywhere a term of type $A$ or type @code{Term} is expected.  The @emph{terms} $A$ and $B$ both
have type @code{Ty}.

@begin[verbatim]
declare typeclass A -> Term
declare typeclass B -> A
@end[verbatim]

In the following example, we define a new typeclass @code{Ty1} that is a subtype of @code{Ty}, and a
new typeclass $C$ that is a subtype of $B$.  The type of the @emph{term} $C$ is @code{Ty1}.  The
type of the term @code{Ty1} is @code{Ty}.

@begin[verbatim]
declare typeclass Ty1 -> Ty
declare typeclass C : Ty1 -> B
@end[verbatim]

@subsubsection[types]{Types and terms}

Types are like typeclasses, but they are more expressive, and they can only be used on the
left-hand-side of a subtyping @code{->}.  Types are declared with @code{declare type}, and terms are
declared with @code{declare} (no modifiers), in the following forms.

@begin[verbatim]
declare type <type> [: type] [-> typeclass]
declare <term> [: type]
@end[verbatim]

For the @code{declare type} form, if the @code{: type} is included, it specifies the type of the
term @code{<type>}.  Otherwise, the term @code{<type>} will have type @code{Ty}.  If @code{-> typeclass}
is included, then the type @code{<type>} is a subtype of @code{typeclass}.

For terms, if the @code{: type} is included, it specifies the type of the @code{<term>}.  Otherwise,
the @code{<term>} will have type @code{Term}.

The @code{<type>} and @code{<term>} are terms that are optionally decorated with types.  The
@code{<type>} terms must be a simple term, meaning it is not allowed to have bindings.

In the following example, we define a type of lists.  The type @code{List} represents a polymorphic
type, where @code{'a} is the type of the elements of the list.  The @code{-> Term} specifies that
any term that is a @code{List} is also a @code{Term}.

@begin[verbatim]
declare type List{'a} -> Term
declare nil : List{'a}
declare cons{'hd : 'a; 'tl : List{'a}} : List{'a}
@end[verbatim]

The @code{nil} term is a list for any type @code{'a}.  The @code{cons} term has two subterms, where
@code{'hd} is a list element, and @code{'tl} is a list.

The terms that denote types must be simple, but we can still define types for terms with binders.

@begin[verbatim]
declare type Fun{'a; 'b}
declare lambda{x : 'a. 'e['x] : 'b} : Fun{'a; 'b}
declare apply{'f : Fun{'a; 'b}; 'e : 'a} : 'b
@end[verbatim]

For a type declaration, if a type constraint is missing in the declaration, it defaults to @code{Ty}.
For a term declaration, it defaults to @code{Term}.  Thus, the above declaration for @code{Fun} is
equivalent to the following declaration.

@begin[verbatim]
declare type Fun{'a : Ty; 'b : Ty} : Ty
@end[verbatim]

@end[doc]
>>

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * fill-column: 100
 * Caml-master: "compile"
 * End:
 * -*-
 *)
