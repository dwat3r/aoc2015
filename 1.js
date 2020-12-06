const input = document.querySelector("pre").textContent
// first part
[...input].reduce((acc,x) => x === "(" ? acc + 1 : acc - 1,0)
// second part
[...input].reduce((acc,x) => ({l:(x === "(" ? acc.l + 1 : acc.l - 1),i:(acc.found || acc.l === -1 ? acc.i : acc.i+1),found: (acc.found || acc.l === -1)}),{l:0,i:0,found:false}).i
