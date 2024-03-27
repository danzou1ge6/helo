import{T as p,a}from"./TextBody1-BPCkxQmK.js";import{E as d,T as _}from"./TextHeader2-TQiaOL02.js";import{T as n}from"./TextCode-DmvwjM7D.js";import{d as h,r as i,a as c,f as g,b as l,w as t,F as x,e,j as u,_ as y}from"./index-DHLu7VNo.js";import"./QBtn-DKy_yooC.js";import"./QCard-e3BSHyBw.js";import"./QScrollObserver-C9H9M275.js";import"./PlayGround-DJiHJlkP.js";const b=u("li",null," At top level of a module, that is at same level as a function definition. In this case, all expressions in the module, regardless of sequence of appearing, are affected by the command. ",-1),w=h({__name:"RefModule",setup(T){const m=i(`module a
    fn n = 1

    module a1
        fn n1 = 2
    end
end

routine main =
    use ops in
    let x1 = a.n in
    let x2 = a.a1.n1 in
    io.println (fmt.to_string x1)  * " " * (fmt.to_string x2)
`),f=i(`module a
    fn n = 1

    module a1
        fn n1 = 2
    end
end

routine main =
    use ops in
    use a in
    use a1 of n1 as n2 in
    let x1 = n in
    let x2 = n2 in
    io.println (fmt.to_string x1)  * " " * (fmt.to_string x2)
`),r=i(`
module a
    fn n = 1
end

use a

fn n = 1

routine main = n
`);return(V,o)=>(c(),g(x,null,[l(p,null,{default:t(()=>[e("Module")]),_:1}),l(a,null,{default:t(()=>[e(" We have a simple module system to aid organizing code. What this module system solely does is seperating namespaces, to enable more concise naming. Unfortunately, this module system doesn't act as compilation unit, nor is higher ranked module supported. ")]),_:1}),l(a,null,{default:t(()=>[e(" Each source file can also be seperated into multiple modueles by "),l(n,null,{default:t(()=>[e("module")]),_:1}),e(" keyword. ")]),_:1}),l(d,{modelValue:m.value,"onUpdate:modelValue":o[0]||(o[0]=s=>m.value=s),height:300},null,8,["modelValue"]),l(_,null,{default:t(()=>[e("Importing Modules")]),_:1}),l(a,null,{default:t(()=>[e(" Modules can be imported using the "),l(n,null,{default:t(()=>[e("use")]),_:1}),e(" keyword. There are two variants of "),l(n,null,{default:t(()=>[e("use")]),_:1}),e(" commands. "),u("ul",null,[u("li",null,[l(n,null,{default:t(()=>[e("use module_name")]),_:1}),e(" imports all symbols from "),l(n,null,{default:t(()=>[e("module_name")]),_:1})]),u("li",null,[l(n,null,{default:t(()=>[e("use module_name of f1, f2, f3 as f")]),_:1}),e(" imports "),l(n,null,{default:t(()=>[e("f1, f2")]),_:1}),e(" from "),l(n,null,{default:t(()=>[e("module_name")]),_:1}),e(", as well as importing "),l(n,null,{default:t(()=>[e("f3")]),_:1}),e(" with the name "),l(n,null,{default:t(()=>[e("f")]),_:1}),e(". ")])]),l(n,null,{default:t(()=>[e("use")]),_:1}),e(" commands can appear in two places "),u("ul",null,[b,u("li",null,[e(" In a "),l(n,null,{default:t(()=>[e("use")]),_:1}),e(" expression, in the form of "),l(n,null,{default:t(()=>[e("use module_name in ...")]),_:1}),e(". In this case, only expressions after "),l(n,null,{default:t(()=>[e("in")]),_:1}),e(" are affected. ")])])]),_:1}),l(d,{modelValue:f.value,"onUpdate:modelValue":o[1]||(o[1]=s=>f.value=s),height:400},null,8,["modelValue"]),l(a,null,{default:t(()=>[e(' We adopt a rather strict attitude towards symbol naming. Two symbols with the same imported name are not allowed to coexist in a module, so be careful with "dumping in" imports in the form of '),l(n,null,{default:t(()=>[e("use module_name")]),_:1}),e(". ")]),_:1}),l(d,{modelValue:r.value,"onUpdate:modelValue":o[2]||(o[2]=s=>r.value=s),height:300,"initial-split":30},null,8,["modelValue"]),l(_,null,{default:t(()=>[e("Modules in File System")]),_:1}),l(a,null,{default:t(()=>[e(" Where module system comes most handy is that it allows splitting large code base into smaller files. The module tree is almost identical to the file tree that it's built from, except in that a "),l(n,null,{default:t(()=>[e("mod.helo")]),_:1}),e(" file is considered in the same module as its parent directory. ")]),_:1}),l(a,null,{default:t(()=>[e(" When "),l(n,null,{default:t(()=>[e("heloc")]),_:1}),e(" is fed a file, it checks if the file is called "),l(n,null,{default:t(()=>[e("mod.helo")]),_:1}),e(". If so, it process are siblings to this "),l(n,null,{default:t(()=>[e("mod.helo")]),_:1}),e(", and put their sub module trees under "),l(n,null,{default:t(()=>[e("/parent_name")]),_:1}),e(" where "),l(n,null,{default:t(()=>[e("parent_name")]),_:1}),e(" is name of "),l(n,null,{default:t(()=>[e("mod.helo")]),_:1}),e("'s parent directory. The content of "),l(n,null,{default:t(()=>[e("mod.helo")]),_:1}),e(" is put directly under "),l(n,null,{default:t(()=>[e("/parent_name")]),_:1}),e(". ")]),_:1}),l(a,null,{default:t(()=>[e(" If the name fed to "),l(n,null,{default:t(()=>[e("heloc")]),_:1}),e(" is not called "),l(n,null,{default:t(()=>[e("mod.helo")]),_:1}),e(", then its stem name (Let's call it "),l(n,null,{default:t(()=>[e("file_name")]),_:1}),e(") is used as the root. All sub module trees from files in a directory next to "),l(n,null,{default:t(()=>[e("file_name.helo")]),_:1}),e(" with identical name is put under "),l(n,null,{default:t(()=>[e("/file_name")]),_:1}),e(", and the contents of "),l(n,null,{default:t(()=>[e("file_name.helo")]),_:1}),e(" goes directly to "),l(n,null,{default:t(()=>[e("/file_name")]),_:1}),e(". ")]),_:1})],64))}}),U=y(w,[["__file","RefModule.vue"]]);export{U as default};
