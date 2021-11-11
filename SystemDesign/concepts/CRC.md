---

excalidraw-plugin: raw

---
==⚠  Switch to EXCALIDRAW VIEW in the MORE OPTIONS menu of this document. ⚠==


# Algorithm

data bits $a=(a_0, \cdots, a_m)$

$$
	M(x) = \sum_{i=0}^m a_i x^i
$$

check bits

$$
	x^rM(x) \mod G(x)
$$

codeword

$$
	x^rM(x) - (x^rM(x) \mod G(x))
$$


# Text Elements
110001 ^QSwlpj3Y

1x^5 + 1x^4 + 0x^3 + 0x^2 + 0x^1 + 1x^0 ^St6mC9Ru

m bits ^e0AfZXri

Polynomial in F_2 ^mvFaTjVC

M(x) ^FIfc0cjs

1. Let r be the degree of G(x). 
  Append r zero bits to the low-order end of the frame 
  so it now contains m + r bits and corresponds to the
  polynomial x^rM(x).
2. Divide the bit string corresponding to G(x) into the bit string corresponding to 
   x^rM(x), using modulo 2 division
3. Subtract the remainder (which is always r or fewer bits) from the bit string
   corresponding to x^rM(x) using modulo 2 subtraction.
   The result is the checksummed frame to be transmitted. Call its polynomial T(x). ^VIjNLVRu

checkbits = x^rM(x) mod G(x) ^FAiV7GnD

codeword = x^rM(x) + (x^rM(x) % G(x)) ^dkzhSQkW

%%
# Drawing
```json
{"type":"excalidraw","version":2,"source":"https://excalidraw.com","elements":[{"id":"QSwlpj3Y","type":"text","x":-499.5,"y":-186.66668701171875,"width":57,"height":25,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":189283685,"version":172,"versionNonce":1834537221,"isDeleted":false,"boundElementIds":["6oE4_zvaT5l2ZHj877nPH"],"text":"110001","fontSize":20,"fontFamily":1,"textAlign":"left","verticalAlign":"top","baseline":18},{"id":"St6mC9Ru","type":"text","x":-676.8334045410156,"y":-48.3333740234375,"width":457,"height":24,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":2114165637,"version":197,"versionNonce":42100837,"isDeleted":false,"boundElementIds":["6oE4_zvaT5l2ZHj877nPH"],"text":"1x^5 + 1x^4 + 0x^3 + 0x^2 + 0x^1 + 1x^0","fontSize":20,"fontFamily":3,"textAlign":"left","verticalAlign":"top","baseline":20},{"id":"e0AfZXri","type":"text","x":276.49993896484375,"y":-169.66668701171875,"width":70,"height":24,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":1613561093,"version":34,"versionNonce":835783397,"isDeleted":false,"boundElementIds":["cE_X7j5I3bAw6sBDYl5GH"],"text":"m bits","fontSize":20,"fontFamily":3,"textAlign":"left","verticalAlign":"top","baseline":20},{"id":"6oE4_zvaT5l2ZHj877nPH","type":"arrow","x":-472.1667175292969,"y":-153,"width":1.33331298828125,"height":94,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"round","seed":548289445,"version":327,"versionNonce":1310485963,"isDeleted":false,"boundElementIds":null,"points":[[0,0],[-1.33331298828125,94]],"lastCommittedPoint":null,"startBinding":{"elementId":"QSwlpj3Y","focus":0.030215025371476165,"gap":8.66668701171875},"endBinding":{"elementId":"St6mC9Ru","focus":-0.11146241339029737,"gap":10.6666259765625},"startArrowhead":null,"endArrowhead":"arrow"},{"id":"mvFaTjVC","type":"text","x":242.49993896484375,"y":-30.33331298828125,"width":171,"height":25,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":502799525,"version":64,"versionNonce":1670932037,"isDeleted":false,"boundElementIds":["cE_X7j5I3bAw6sBDYl5GH"],"text":"Polynomial in F_2","fontSize":20,"fontFamily":1,"textAlign":"left","verticalAlign":"top","baseline":18},{"id":"cE_X7j5I3bAw6sBDYl5GH","type":"arrow","x":311.83331298828125,"y":-137,"width":0,"height":98.66668701171875,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"round","seed":1380871979,"version":60,"versionNonce":1472545259,"isDeleted":false,"boundElementIds":null,"points":[[0,0],[0,98.66668701171875]],"lastCommittedPoint":null,"startBinding":{"elementId":"e0AfZXri","focus":-0.009524972098214287,"gap":8.66668701171875},"endBinding":{"elementId":"mvFaTjVC","focus":-0.18908334475511698,"gap":8},"startArrowhead":null,"endArrowhead":"arrow"},{"id":"FIfc0cjs","type":"text","x":-367.8333740234375,"y":62.000030517578125,"width":41,"height":25,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":1676653579,"version":193,"versionNonce":1687659589,"isDeleted":false,"boundElementIds":null,"text":"M(x)","fontSize":20,"fontFamily":1,"textAlign":"left","verticalAlign":"top","baseline":18},{"id":"VIjNLVRu","type":"text","x":-694.166748046875,"y":-598.0000152587891,"width":835,"height":228,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":1727569349,"version":364,"versionNonce":221731045,"isDeleted":false,"boundElementIds":null,"text":"1. Let r be the degree of G(x). \n  Append r zero bits to the low-order end of the frame \n  so it now contains m + r bits and corresponds to the\n  polynomial x^rM(x).\n2. Divide the bit string corresponding to G(x) into the bit string corresponding to \n   x^rM(x), using modulo 2 division\n3. Subtract the remainder (which is always r or fewer bits) from the bit string\n   corresponding to x^rM(x) using modulo 2 subtraction.\n   The result is the checksummed frame to be transmitted. Call its polynomial T(x).","fontSize":20,"fontFamily":1,"textAlign":"left","verticalAlign":"top","baseline":221},{"id":"FAiV7GnD","type":"text","x":-455.83343505859375,"y":128.66671752929688,"width":289,"height":25,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":1628153771,"version":34,"versionNonce":1686498917,"isDeleted":false,"boundElementIds":null,"text":"checkbits = x^rM(x) mod G(x)","fontSize":20,"fontFamily":1,"textAlign":"left","verticalAlign":"top","baseline":18},{"id":"dkzhSQkW","type":"text","x":-455.166748046875,"y":172.66671752929688,"width":392,"height":25,"angle":0,"strokeColor":"#000000","backgroundColor":"transparent","fillStyle":"hachure","strokeWidth":1,"strokeStyle":"solid","roughness":1,"opacity":100,"groupIds":[],"strokeSharpness":"sharp","seed":242412677,"version":51,"versionNonce":438010507,"isDeleted":false,"boundElementIds":null,"text":"codeword = x^rM(x) + (x^rM(x) % G(x))","fontSize":20,"fontFamily":1,"textAlign":"left","verticalAlign":"top","baseline":18}],"appState":{"theme":"light","viewBackgroundColor":"#ffffff","currentItemStrokeColor":"#000000","currentItemBackgroundColor":"transparent","currentItemFillStyle":"hachure","currentItemStrokeWidth":1,"currentItemStrokeStyle":"solid","currentItemRoughness":1,"currentItemOpacity":100,"currentItemFontFamily":1,"currentItemFontSize":20,"currentItemTextAlign":"left","currentItemStrokeSharpness":"sharp","currentItemStartArrowhead":null,"currentItemEndArrowhead":"arrow","currentItemLinearStrokeSharpness":"round","gridSize":null}}
```%%