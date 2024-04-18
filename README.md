# Pure-rust

**Warning! ABANDONWARE!**

A strongly typed HTML generation language that uses rust's type system, and the official specification of HTML from
 the w3c, to make sure you can't build invalid HTML documents. 
 
 It also allows you to create your own components -- similar to React components -- that compose together nicely to
  create web apps.
  
 # Design Philosophy
 
 As a jobbing web programmer, generating HTML is your bread and butter, and the more the compiler can do to
  help you, the better. Some of us work in webassembly client-side apps, and some of us in server-side rendering
  ; pure-rust is designed to give you both. 

*Standards-based:* I generate the HTML library from the w3c's own definition; every element type becomes a struct (eg
 `<p>` -> `struct P {...}`) and every attribute with fixed possibilities becomes an enum (`enum InputType { Text
 , Submit, Hidden, ... }`) so you get the full power of your tools. 
  
*Build your own design system:* we rarely actually code pure HTML. Normally there is some design system involved - a
 bootstrap, material design, or corporate styles. pure-html is designed as the raw building material you need to
  encode and share design systems without having to become a master macro writer.
  
*Simple Function Composition:* I also wanted to avoid using macros; my observation is that macro-based systems become
  highly opinionated about what
  they allow. By keeping things very pure in terms of writing functions which call functions and compose neatly, I'm
   hoping to avoid telling you how you should program.
   
