import{Q as c}from"./QBtn-DCnwbdmd.js";import{T as h,a as i}from"./TextBody1-BweaWQnZ.js";import{E as l,T as f}from"./TextHeader2-JLM94Jsm.js";import{T as s}from"./TextCode-CDh-EaCl.js";import{d as x,r,a as y,f as b,b as t,w as n,F as _,e,_ as I}from"./index-tVr8bQrc.js";import"./QCard-DgAS53pM.js";import"./QScrollObserver-CNI9KVLI.js";import"./PlayGround-ByfgStqG.js";const p=`/* 'b is dependent on 'a */
relation Iterator 'a,'b -> 'b
    fn next it: ['a] -> ('a, 'b)
end

data IntIterator = IntIterator Int

instance Iterator IntIterator,Int
    fn next it =
        let IntIterator x = it in
        use ops of (+) in
        ((IntIterator x + 1), x)
end
`,w=x({__name:"RefRelation",setup(R){const d=r(p+`
routine main =
    let it = IntIterator 1 in
    let (it, x1) = next it in
    let (it, x2) = next it in
    use ops of (*) in
    io.println (fmt.to_string x1) * " " * (fmt.to_string x2)
`),u=r(p+`
fn take 'a,'b it,n: ['a, Int] -> list.List['b] where Iterator 'a,'b =
    use list in
    use ops of (-),(==) in
    if n == 0 then
        []
    else
        let (it, x) = next it in
        x :: (take it,n - 1)

fn debug_int_list xs: [list.List[Int]] -> Str =
    use ops of (*) in
    use list in
    case xs of
    | x :: xs -> (fmt.to_string x) * " " * (debug_int_list xs)
    | [] -> ""
    end

routine main =
    io.println debug_int_list take (IntIterator 1),3
`),m=r(`relation A 'a end
relation B 'a where A 'a end

instance B Int end

routine main = 0
`);return(g,a)=>(y(),b(_,null,[t(h,null,{default:n(()=>[e("Relations")]),_:1}),t(i,null,{default:n(()=>[e(' To allow ad-hoc polymorphism, we borrow the mathmatical concept of "relation". If some methods are implemented for a series of types, then those types satisfy some certain relation. Those relations are defined by the '),t(s,null,{default:n(()=>[e("relation")]),_:1}),e(" keyword, and those methods are implemented for some concrete types through the "),t(s,null,{default:n(()=>[e("instance")]),_:1}),e(" keyword. ")]),_:1}),t(l,{modelValue:d.value,"onUpdate:modelValue":a[0]||(a[0]=o=>d.value=o),height:500},null,8,["modelValue"]),t(f,null,{default:n(()=>[e("Dependent")]),_:1}),t(i,null,{default:n(()=>[e(" Sometimes, for example, for a relation R(x, y, z), z can be uniquely determined from the value of x and y, then z is called dependent on other variables. In this case, if the inferer knows the concrete type of x and y, it can know the type of z even z has not been infered. To express this, we use a "),t(s,null,{default:n(()=>[e("->")]),_:1}),e(" syntax as shown above. ")]),_:1}),t(f,null,{default:n(()=>[e("Constrains")]),_:1}),t(i,null,{default:n(()=>[e(" Generic type variables in a relation, instance or function can be constrained. That is, we can say that some type variables must satisfy certain relations, then we can use relevant methods in the function body. ")]),_:1}),t(l,{modelValue:u.value,"onUpdate:modelValue":a[1]||(a[1]=o=>u.value=o),height:500},null,8,["modelValue"]),t(i,null,{default:n(()=>[e(" Not only can constrains be added to function signature, but they can also be a part of instance definition or relation definition. A constrain R(x, y) on a relation means that to implement this relation for some types x = a, y = b, R(a, b) must also be implemented. A constrain R(a, b) on a instance means that this instance is implemented for all types a, b that satisfies R. ")]),_:1}),t(l,{modelValue:m.value,"onUpdate:modelValue":a[2]||(a[2]=o=>m.value=o),height:300,"initial-split":30},null,8,["modelValue"]),t(i,null,{default:n(()=>[e(" Refer to "),t(c,{flat:"",color:"primary",dense:"","no-caps":"",to:"/playground/RelationExample"},{default:n(()=>[e("Relation Example")]),_:1}),e(" in playground for a more complicated example. ")]),_:1})],64))}}),A=I(w,[["__file","RefRelation.vue"]]);export{A as default};
