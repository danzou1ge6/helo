import{T as c,a as u}from"./TextBody1-edBUOYSb.js";import{E as a,T as p}from"./TextHeader2-h60F0uur.js";import{T as r}from"./TextCode-LJP9iye8.js";import{d as x,r as s,a as _,f as y,b as e,w as l,F as h,e as t,_ as v}from"./index-Cq87Jjhi.js";import"./QBtn-B628EINK.js";import"./QCard-B49l-k4v.js";import"./QScrollObserver-CRQL0R-Z.js";import"./PlayGround-Clh7w6at.js";const C=x({__name:"RefClosures",setup(g){const i=s(`fn f x =
    use arith.int of (+) in
    let fn g y: [Int] -> Int = x + y in g

routine r x =
    use ops of (*) in
    let routine p y =
      io.println (fmt.int_to_str x)
                 * " "
                 * (fmt.int_to_str y)
    in p

routine main =
    let g = f 1 in
    let z = g 2 in
    let p = r 3 in
    p z
`),d=s(`routine main =
    use arith.int in
    let fn factorial n =
        if n == 0 then 1 else n * (factorial n - 1)
    in
    factorial 4
`),f=s(`fn f x =
    use arith.int of (+) in
    \\y: [Int] -> Int -> x + y

routine r x =
    use ops of (*) in
    \\'y -> io.println (fmt.int_to_str x)
                       * " "
                       * (fmt.int_to_str y)

routine main =
    let g = f 1 in
    let z = g 2 in
    let p = r 3 in
    p z
`),m=s(`fn f x =
    \\y: [Int] -> [Int] -> Int ->
    /* closure above must also captures x because
     * its "child closure" captures x
     */
        \\z -> x  /* x here refer to f's parameter */

routine main =
    let h = (f 1) 2 in
    h 3
`);return(V,n)=>(_(),y(h,null,[e(c,null,{default:l(()=>[t("Closures")]),_:1}),e(u,null,{default:l(()=>[t(" A closures captures locals in its surrounding function by refering to them, and store those values in their environment. "),e(r,null,{default:l(()=>[t("let fn")]),_:1}),t(" and "),e(r,null,{default:l(()=>[t("let routine")]),_:1}),t(" keyword can be used to define pure and impure closures, respectively. ")]),_:1}),e(a,{modelValue:i.value,"onUpdate:modelValue":n[0]||(n[0]=o=>i.value=o),height:400},null,8,["modelValue"]),e(u,null,{default:l(()=>[t(" A named closure can be recursive. ")]),_:1}),e(a,{modelValue:d.value,"onUpdate:modelValue":n[1]||(n[1]=o=>d.value=o),height:300},null,8,["modelValue"]),e(p,null,{default:l(()=>[t("Anonymous Closures")]),_:1}),e(u,null,{default:l(()=>[t(" To avoid naming a closure, anonymous closure can be created with "),e(r,null,{default:l(()=>[t("\\x,y -> ...")]),_:1}),t(" syntax. For the impure case, that would be "),e(r,null,{default:l(()=>[t("\\'x,y -> ...")]),_:1}),t(". ")]),_:1}),e(a,{modelValue:f.value,"onUpdate:modelValue":n[2]||(n[2]=o=>f.value=o),height:400},null,8,["modelValue"]),e(u,null,{default:l(()=>[t(" As shown above, type annotation can be supplied to closure as well. A closure shares the same generic type parameters as its parent. ")]),_:1}),e(p,null,{default:l(()=>[t("Nested Closures")]),_:1}),e(u,null,{default:l(()=>[t(" Closures can be nested. ")]),_:1}),e(a,{modelValue:m.value,"onUpdate:modelValue":n[3]||(n[3]=o=>m.value=o),height:300},null,8,["modelValue"])],64))}}),B=v(C,[["__file","RefClosures.vue"]]);export{B as default};
