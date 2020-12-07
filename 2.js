let input = document.querySelector("pre").textContent.split("\n").filter(i => i !== "").map(i => i.split("x").map(i => parseInt(i)))
//first part
input.map(i => {let [l,w,h] = i.sort((a,b) => a - b);return (l * w + 2*l*w + 2*w*h + 2*h*l)}).reduce((acc,x) => acc + x)
//second part
input.map(i => {let [l,w,h] = i.sort((a,b) => a - b);return (l*w*h + 2*l + 2*w)}).reduce((acc,x) => acc + x)
