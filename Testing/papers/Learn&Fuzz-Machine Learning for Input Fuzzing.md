---

excalidraw-plugin: raw

---
==⚠  Switch to EXCALIDRAW VIEW in the MORE OPTIONS menu of this document. ⚠==


# Text Elements
AIM: Generate good input that can go deeper (pass the parser) ^j0Bs6OKa

Case Study: PDF format ^10bLoBxw

obj_id obj_ver START_FLAG f3728hq89f2@j89fw END_FLAG  ^8aEk9zRZ

can contain array, dict, ref ^VethNhlG

PDF body ^xVn8J5eR

Objects ^U8nwcbLt

Cross-reference-table ^86WhdZAV

Trailer ^bwE38C3m

PDF body ^Jd4rIOBt

Objects ^otPJatX7

Cross-reference-table ^dyPU1gYE

Trailer ^7kgO5GPv

PDF File ^RSUmySZ8

IDEA: Use a RNN model to 
Learn the PDF gramma of objects ^hc5qua2X

In this paper, we investigate how to leverage and adapt
neural-network-based learning techniques to learn a grammar for non-binary
PDF data objects. ^52yMCu60

Rules for defining and composing such data objects makes
the bulk of the 1,300-pages PDF-format specification. These rules are numerous
and tedious, but repetitive and structured, and therefore well-suited for learning
with neural networks (as we will show later). ^lYp50ZDV

why NN works ^vC5YzC6M

Sampling Strategy ^0opg0NZS

1. no-sampling: MAP ^fCJMSlrA

argmax Pr(x[k+1] | x[1:k]) ^is7YUVOH

2. sampling ^9DIvXupV

Learn&Fuzz: Machine Learning for Input Fuzzing ^3qxWdYCq

x[k] ~ Pr( · | x[1:k]) ^loVijShF

It samples the distribution to generate the next character only when
the current prefix sequence ends with a whitespace ^Xvyyrh6E

3. sample-space ^wBNtBcIG

Observations ^9xPWlINJ

A perfect learning technique would always generate well-formed
objects that would not exercise any error-handling code, 
whereas a bad learning technique would result in ill-formed objects that will be quickly rejected
by the parser upfront ^oomC8UZg

too good  -> no-error (exercise the same path)
too bad   -> can't go deeper ^RnAdCmuk

Pr( · | x[1:k]) ^pt06e2zl

c, Pr(c) ^ebErNtQb

sample ^s7MQ7mCu

Pr(c) > threshold? ^1xtdu5Hf

Y: resample ^BFo7n3xb

x[k] = c ^c3TJfGG2

IDEA: if the model is too confident of a sample then 
the sample result may be a travail one ^HfCDNx3p

%%
# Drawing
```json
{
	"type": "excalidraw",
	"version": 2,
	"source": "https://excalidraw.com",
	"elements": [
		{
			"type": "text",
			"version": 128,
			"versionNonce": 110363785,
			"isDeleted": false,
			"id": "j0Bs6OKa",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -351.66668701171875,
			"y": -247.8333282470703,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 899,
			"height": 35,
			"seed": 1313708329,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "AIM: Generate good input that can go deeper (pass the parser)",
			"rawText": "AIM: Generate good input that can go deeper (pass the parser)",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 49,
			"versionNonce": 1153026631,
			"isDeleted": false,
			"id": "10bLoBxw",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -350,
			"y": -177.5,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 343,
			"height": 35,
			"seed": 1900274375,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "Case Study: PDF format",
			"rawText": "Case Study: PDF format",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 608,
			"versionNonce": 470030759,
			"isDeleted": false,
			"id": "8aEk9zRZ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 152.96287706163196,
			"y": -62.944403754340385,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 497,
			"height": 19,
			"seed": 483418215,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"x2VN3-sX85UvdLu0DV5MJ"
			],
			"fontSize": 16,
			"fontFamily": 3,
			"text": "obj_id obj_ver START_FLAG f3728hq89f2@j89fw END_FLAG ",
			"rawText": "obj_id obj_ver START_FLAG f3728hq89f2@j89fw END_FLAG ",
			"baseline": 16,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 978,
			"versionNonce": 801131719,
			"isDeleted": false,
			"id": "x2VN3-sX85UvdLu0DV5MJ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 204.71044728350924,
			"y": 43.18009625262093,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 207.98157175623422,
			"height": 84.5483172059409,
			"seed": 2064866503,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "VethNhlG",
				"focus": -1.0225543516428508,
				"gap": 1.87554833619879
			},
			"endBinding": {
				"elementId": "8aEk9zRZ",
				"focus": -0.1505706180906392,
				"gap": 2.576182801020405
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					207.98157175623422,
					-84.5483172059409
				]
			]
		},
		{
			"type": "text",
			"version": 406,
			"versionNonce": 155653703,
			"isDeleted": false,
			"id": "VethNhlG",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 203.14994371160742,
			"y": 45.05564458881972,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 217,
			"height": 20,
			"seed": 1351984263,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"x2VN3-sX85UvdLu0DV5MJ"
			],
			"fontSize": 16,
			"fontFamily": 1,
			"text": "can contain array, dict, ref",
			"rawText": "can contain array, dict, ref",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "rectangle",
			"version": 229,
			"versionNonce": 511922921,
			"isDeleted": false,
			"id": "94-i8D_i_3E7YqSLW7w1G",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -273.8484374976846,
			"y": -80.16604184712565,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 226.4789573566696,
			"height": 85.6172351415575,
			"seed": 352808073,
			"groupIds": [
				"zAJHOaX_dh8yy-Y3EaMu7",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": []
		},
		{
			"type": "text",
			"version": 147,
			"versionNonce": 2084465703,
			"isDeleted": false,
			"id": "xVn8J5eR",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -254.71806285409622,
			"y": -68.47528977674963,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 73,
			"height": 20,
			"seed": 1565395081,
			"groupIds": [
				"zAJHOaX_dh8yy-Y3EaMu7",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 15.941978869656875,
			"fontFamily": 1,
			"text": "PDF body",
			"rawText": "PDF body",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 132,
			"versionNonce": 1045106121,
			"isDeleted": false,
			"id": "U8nwcbLt",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -226.4602887328311,
			"y": -40.941393718608275,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 45,
			"height": 15,
			"seed": 490865479,
			"groupIds": [
				"zAJHOaX_dh8yy-Y3EaMu7",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 11.946595189300496,
			"fontFamily": 1,
			"text": "Objects",
			"rawText": "Objects",
			"baseline": 11,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 164,
			"versionNonce": 1779191623,
			"isDeleted": false,
			"id": "86WhdZAV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -224.32410885687545,
			"y": -18.93975061648942,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 127,
			"height": 15,
			"seed": 1534164967,
			"groupIds": [
				"zAJHOaX_dh8yy-Y3EaMu7",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 11.946595189300487,
			"fontFamily": 1,
			"text": "Cross-reference-table",
			"rawText": "Cross-reference-table",
			"baseline": 11,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "text",
			"version": 127,
			"versionNonce": 1192783017,
			"isDeleted": false,
			"id": "bwE38C3m",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -160.70852593196568,
			"y": -41.24006771286957,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 40,
			"height": 15,
			"seed": 1230846279,
			"groupIds": [
				"zAJHOaX_dh8yy-Y3EaMu7",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 11.94659518930048,
			"fontFamily": 1,
			"text": "Trailer",
			"rawText": "Trailer",
			"baseline": 11,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "rectangle",
			"version": 358,
			"versionNonce": 644927079,
			"isDeleted": false,
			"id": "wrkSb2_pEWLdys6zQZjZN",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -273.9934938700127,
			"y": 14.212069262877208,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 226.4789573566696,
			"height": 85.6172351415575,
			"seed": 1022401321,
			"groupIds": [
				"0opD3Gl3kAnYA4gJ6CVMW",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": []
		},
		{
			"type": "text",
			"version": 274,
			"versionNonce": 1352069001,
			"isDeleted": false,
			"id": "Jd4rIOBt",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -254.863119226425,
			"y": 25.902821333253044,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 73,
			"height": 20,
			"seed": 1012451303,
			"groupIds": [
				"0opD3Gl3kAnYA4gJ6CVMW",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 15.941978869656875,
			"fontFamily": 1,
			"text": "PDF body",
			"rawText": "PDF body",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 259,
			"versionNonce": 1547200903,
			"isDeleted": false,
			"id": "otPJatX7",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -226.60534510515953,
			"y": 53.436717391394325,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 45,
			"height": 15,
			"seed": 1751469577,
			"groupIds": [
				"0opD3Gl3kAnYA4gJ6CVMW",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 11.946595189300496,
			"fontFamily": 1,
			"text": "Objects",
			"rawText": "Objects",
			"baseline": 11,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 291,
			"versionNonce": 1895962217,
			"isDeleted": false,
			"id": "dyPU1gYE",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -224.4691652292043,
			"y": 75.43836049351327,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 127,
			"height": 15,
			"seed": 904213255,
			"groupIds": [
				"0opD3Gl3kAnYA4gJ6CVMW",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 11.946595189300487,
			"fontFamily": 1,
			"text": "Cross-reference-table",
			"rawText": "Cross-reference-table",
			"baseline": 11,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "text",
			"version": 254,
			"versionNonce": 681549991,
			"isDeleted": false,
			"id": "7kgO5GPv",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -160.8535823042943,
			"y": 53.13804339713295,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 40,
			"height": 15,
			"seed": 1820370153,
			"groupIds": [
				"0opD3Gl3kAnYA4gJ6CVMW",
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 11.94659518930048,
			"fontFamily": 1,
			"text": "Trailer",
			"rawText": "Trailer",
			"baseline": 11,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "rectangle",
			"version": 162,
			"versionNonce": 1797611849,
			"isDeleted": false,
			"id": "JN8ITVhTs1skYLR0MOrUM",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -292.96302625868066,
			"y": -94.50196518881515,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 410.9629109700521,
			"height": 216.63161403321618,
			"seed": 1144765417,
			"groupIds": [
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": []
		},
		{
			"type": "text",
			"version": 266,
			"versionNonce": 1297281991,
			"isDeleted": false,
			"id": "RSUmySZ8",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -10.02781803937572,
			"y": 57.020705062713404,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 89,
			"height": 27,
			"seed": 1721292231,
			"groupIds": [
				"mA9ajF9JuAi7XLYj-Pmrf"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"jQEWyhtHgB8fPcRiGSeiP"
			],
			"fontSize": 21.503871340740883,
			"fontFamily": 1,
			"text": "PDF File",
			"rawText": "PDF File",
			"baseline": 20,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 1147,
			"versionNonce": 292398119,
			"isDeleted": false,
			"id": "jQEWyhtHgB8fPcRiGSeiP",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -66.33198696500992,
			"y": 152.61057233746783,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 102.25729632667758,
			"height": 65.72336107244558,
			"seed": 108154279,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "hc5qua2X",
				"gap": 3.18574656444173,
				"focus": -0.07897113970525481
			},
			"endBinding": {
				"elementId": "RSUmySZ8",
				"gap": 2.986667026382723,
				"focus": -0.41939912094677806
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					102.25729632667758,
					-65.72336107244558
				]
			]
		},
		{
			"type": "text",
			"version": 533,
			"versionNonce": 1264639239,
			"isDeleted": false,
			"id": "hc5qua2X",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340.5185885959203,
			"y": 155.79631890190956,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 475,
			"height": 71,
			"seed": 17839465,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"jQEWyhtHgB8fPcRiGSeiP"
			],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "IDEA: Use a RNN model to \nLearn the PDF gramma of objects",
			"rawText": "IDEA: Use a RNN model to \nLearn the PDF gramma of objects",
			"baseline": 60,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 196,
			"versionNonce": 2133474057,
			"isDeleted": false,
			"id": "52yMCu60",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -315.62963189019143,
			"y": 291.2221544053821,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 541,
			"height": 56,
			"seed": 1949801639,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 16,
			"fontFamily": 2,
			"text": "In this paper, we investigate how to leverage and adapt\nneural-network-based learning techniques to learn a grammar for non-binary\nPDF data objects.",
			"rawText": "In this paper, we investigate how to leverage and adapt\nneural-network-based learning techniques to learn a grammar for non-binary\nPDF data objects.",
			"baseline": 52,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 59,
			"versionNonce": 1241830119,
			"isDeleted": false,
			"id": "lYp50ZDV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -314.14824083116343,
			"y": 380.38888888888886,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 569,
			"height": 75,
			"seed": 610431047,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"6mEs6CVrni4rSY2NxNfQt"
			],
			"fontSize": 16,
			"fontFamily": 2,
			"text": "Rules for defining and composing such data objects makes\nthe bulk of the 1,300-pages PDF-format specification. These rules are numerous\nand tedious, but repetitive and structured, and therefore well-suited for learning\nwith neural networks (as we will show later).",
			"rawText": "Rules for defining and composing such data objects makes\nthe bulk of the 1,300-pages PDF-format specification. These rules are numerous\nand tedious, but repetitive and structured, and therefore well-suited for learning\nwith neural networks (as we will show later).",
			"baseline": 71,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 200,
			"versionNonce": 1742200327,
			"isDeleted": false,
			"id": "6mEs6CVrni4rSY2NxNfQt",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 364.4714369013984,
			"y": 353.4443766276041,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 103.06412217700608,
			"height": 38.54316724756899,
			"seed": 1770297161,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "vC5YzC6M",
				"focus": 0.21671106201672044,
				"gap": 2.962917751736086
			},
			"endBinding": {
				"elementId": "lYp50ZDV",
				"focus": 0.5764296992345403,
				"gap": 6.5555555555556
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-103.06412217700608,
					38.54316724756899
				]
			]
		},
		{
			"type": "text",
			"version": 76,
			"versionNonce": 1203017127,
			"isDeleted": false,
			"id": "vC5YzC6M",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 362.88870578341977,
			"y": 325.481458875868,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 130,
			"height": 25,
			"seed": 2001189895,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"6mEs6CVrni4rSY2NxNfQt"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "why NN works",
			"rawText": "why NN works",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 58,
			"versionNonce": 1302335241,
			"isDeleted": false,
			"id": "0opg0NZS",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -333.4074774848092,
			"y": 512.5000678168401,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 244,
			"height": 35,
			"seed": 503269447,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "Sampling Strategy",
			"rawText": "Sampling Strategy",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 31,
			"versionNonce": 624658953,
			"isDeleted": false,
			"id": "fCJMSlrA",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -272.6667141384553,
			"y": 577.3148600260414,
			"strokeColor": "#364fc7",
			"backgroundColor": "transparent",
			"width": 182,
			"height": 25,
			"seed": 1044495273,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "1. no-sampling: MAP",
			"rawText": "1. no-sampling: MAP",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 136,
			"versionNonce": 249983273,
			"isDeleted": false,
			"id": "is7YUVOH",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -205.99997965494828,
			"y": 629.9074639214406,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 223,
			"height": 23,
			"seed": 1174456745,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "argmax Pr(x[k+1] | x[1:k])",
			"rawText": "argmax Pr(x[k+1] | x[1:k])",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 42,
			"versionNonce": 1030369929,
			"isDeleted": false,
			"id": "9DIvXupV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -275.2593519422747,
			"y": 674.8334011501735,
			"strokeColor": "#364fc7",
			"backgroundColor": "transparent",
			"width": 105,
			"height": 25,
			"seed": 910515145,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "2. sampling",
			"rawText": "2. sampling",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 75,
			"versionNonce": 319585511,
			"isDeleted": false,
			"id": "3qxWdYCq",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -322.66671413845535,
			"y": -390.8332655164933,
			"strokeColor": "#5f3dc4",
			"backgroundColor": "transparent",
			"width": 849,
			"height": 45,
			"seed": 1212182503,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 36,
			"fontFamily": 1,
			"text": "Learn&Fuzz: Machine Learning for Input Fuzzing",
			"rawText": "Learn&Fuzz: Machine Learning for Input Fuzzing",
			"baseline": 32,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 120,
			"versionNonce": 1616920967,
			"isDeleted": false,
			"id": "loVijShF",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -191.18518744574675,
			"y": 731.0556572808159,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 163,
			"height": 23,
			"seed": 443494983,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "x[k] ~ Pr( · | x[1:k])",
			"rawText": "x[k] ~ Pr( · | x[1:k])",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 46,
			"versionNonce": 1908143015,
			"isDeleted": false,
			"id": "Xvyyrh6E",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -190.81483968098996,
			"y": 818.7964545355901,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 595,
			"height": 45,
			"seed": 1679302695,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "It samples the distribution to generate the next character only when\nthe current prefix sequence ends with a whitespace",
			"rawText": "It samples the distribution to generate the next character only when\nthe current prefix sequence ends with a whitespace",
			"baseline": 41,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 108,
			"versionNonce": 1337874249,
			"isDeleted": false,
			"id": "wBNtBcIG",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -273.68518744574686,
			"y": 771.5927056206596,
			"strokeColor": "#364fc7",
			"backgroundColor": "transparent",
			"width": 155,
			"height": 25,
			"seed": 1354204841,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "3. sample-space",
			"rawText": "3. sample-space",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 112,
			"versionNonce": 1302610055,
			"isDeleted": false,
			"id": "9xPWlINJ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -339.85192192925376,
			"y": 917.7037489149305,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 176,
			"height": 35,
			"seed": 953316839,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "Observations",
			"rawText": "Observations",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 171,
			"versionNonce": 2070370919,
			"isDeleted": false,
			"id": "oomC8UZg",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -256.7408108181428,
			"y": 976.9445122612846,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 834,
			"height": 91,
			"seed": 1967833865,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "A perfect learning technique would always generate well-formed\nobjects that would not exercise any error-handling code, \nwhereas a bad learning technique would result in ill-formed objects that will be quickly rejected\nby the parser upfront",
			"rawText": "A perfect learning technique would always generate well-formed\nobjects that would not exercise any error-handling code, \nwhereas a bad learning technique would result in ill-formed objects that will be quickly rejected\nby the parser upfront",
			"baseline": 86,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 160,
			"versionNonce": 811389959,
			"isDeleted": false,
			"id": "RnAdCmuk",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -259.33338080512186,
			"y": 1097.3149278428818,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 469,
			"height": 51,
			"seed": 509856361,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "too good  -> no-error (exercise the same path)\ntoo bad   -> can't go deeper",
			"rawText": "too good  -> no-error (exercise the same path)\ntoo bad   -> can't go deeper",
			"baseline": 44,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 233,
			"versionNonce": 275607625,
			"isDeleted": false,
			"id": "pt06e2zl",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -187.7037963867192,
			"y": 1235.185343424479,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 109,
			"height": 23,
			"seed": 1084225543,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"qFVzW_6B3X4HWO_VEKfm6",
				"635Fnrs5o6VMMzeCIwO97"
			],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "Pr( · | x[1:k])",
			"rawText": "Pr( · | x[1:k])",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 507,
			"versionNonce": 466255657,
			"isDeleted": false,
			"id": "qFVzW_6B3X4HWO_VEKfm6",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -70.95654262297447,
			"y": 1248.5661395558277,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 299.72629721268095,
			"height": 2.3034029303712487,
			"seed": 1689456455,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "pt06e2zl",
				"focus": 0.11763175094248753,
				"gap": 7.747253763744737
			},
			"endBinding": {
				"elementId": "ebErNtQb",
				"focus": -0.2813141984509143,
				"gap": 10.415405729303757
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					299.72629721268095,
					2.3034029303712487
				]
			]
		},
		{
			"type": "text",
			"version": 162,
			"versionNonce": 10536359,
			"isDeleted": false,
			"id": "ebErNtQb",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 239.18516031901027,
			"y": 1235.8334011501734,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 94,
			"height": 24,
			"seed": 734527657,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"qFVzW_6B3X4HWO_VEKfm6",
				"WAkUn6xk3Cx36SRDpmPhV"
			],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "c, Pr(c)",
			"rawText": "c, Pr(c)",
			"baseline": 20,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 56,
			"versionNonce": 1197547273,
			"isDeleted": false,
			"id": "s7MQ7mCu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 26.855140011212043,
			"y": 1255.79638671875,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 63,
			"height": 25,
			"seed": 1688761577,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "sample",
			"rawText": "sample",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 202,
			"versionNonce": 1794444361,
			"isDeleted": false,
			"id": "WAkUn6xk3Cx36SRDpmPhV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 267.6570004556175,
			"y": 1274.039810709788,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 90.59860793094788,
			"height": 73.92454987015708,
			"seed": 63713673,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "ebErNtQb",
				"focus": -0.22022320419612268,
				"gap": 14.206409559614713
			},
			"endBinding": {
				"elementId": "1D9In_NvdK07h0gXaBA8Z",
				"focus": 0.5743191762822076,
				"gap": 12.96735717413052
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-90.59860793094788,
					73.92454987015708
				]
			]
		},
		{
			"type": "diamond",
			"version": 123,
			"versionNonce": 870807751,
			"isDeleted": false,
			"id": "1D9In_NvdK07h0gXaBA8Z",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -30.44442409939262,
			"y": 1330.7594401041665,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 258.5184733072917,
			"height": 102.9629855685762,
			"seed": 741501897,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"WAkUn6xk3Cx36SRDpmPhV",
				"635Fnrs5o6VMMzeCIwO97",
				"-KMnIUVFJ5jVHkXJ8EhtG"
			]
		},
		{
			"type": "text",
			"version": 41,
			"versionNonce": 247063849,
			"isDeleted": false,
			"id": "1xtdu5Hf",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 28.222242567274037,
			"y": 1369.0001695421006,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 159,
			"height": 23,
			"seed": 205613449,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "Pr(c) > threshold?",
			"rawText": "Pr(c) > threshold?",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 74,
			"versionNonce": 442675721,
			"isDeleted": false,
			"id": "635Fnrs5o6VMMzeCIwO97",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1.7012622389223964,
			"y": 1356.9885336738575,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 121.91829788079153,
			"height": 90.6736058309757,
			"seed": 1042357735,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "1D9In_NvdK07h0gXaBA8Z",
				"focus": -0.4851503046607263,
				"gap": 11.565806435384403
			},
			"endBinding": {
				"elementId": "pt06e2zl",
				"focus": 0.19162875122910103,
				"gap": 8.129584418402828
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-121.91829788079153,
					-90.6736058309757
				]
			]
		},
		{
			"type": "text",
			"version": 43,
			"versionNonce": 1179882857,
			"isDeleted": false,
			"id": "BFo7n3xb",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -139.33338080512186,
			"y": 1313.9815945095484,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 105,
			"height": 23,
			"seed": 292116711,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "Y: resample",
			"rawText": "Y: resample",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 92,
			"versionNonce": 846110215,
			"isDeleted": false,
			"id": "-KMnIUVFJ5jVHkXJ8EhtG",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 98.64356315640897,
			"y": 1440.7389416732065,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 0.9648371512240175,
			"height": 138.1686239734945,
			"seed": 938410057,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "1D9In_NvdK07h0gXaBA8Z",
				"focus": 0.006726215143734021,
				"gap": 7.018605498318408
			},
			"endBinding": {
				"elementId": "c3TJfGG2",
				"focus": 0.21943104006994416,
				"gap": 2.481526692708485
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0.9648371512240175,
					138.1686239734945
				]
			]
		},
		{
			"type": "text",
			"version": 58,
			"versionNonce": 1349751527,
			"isDeleted": false,
			"id": "c3TJfGG2",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 60.666619194878194,
			"y": 1581.3890923394094,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 64,
			"height": 23,
			"seed": 880201447,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"-KMnIUVFJ5jVHkXJ8EhtG"
			],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "x[k] = c",
			"rawText": "x[k] = c",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 277,
			"versionNonce": 2118242729,
			"isDeleted": false,
			"id": "HfCDNx3p",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 181.0369669596351,
			"y": 1446.2038167317705,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 432,
			"height": 40,
			"seed": 494351111,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 16,
			"fontFamily": 1,
			"text": "IDEA: if the model is too confident of a sample then \nthe sample result may be a travail one",
			"rawText": "IDEA: if the model is too confident of a sample then \nthe sample result may be a travail one",
			"baseline": 34,
			"textAlign": "left",
			"verticalAlign": "top"
		}
	],
	"appState": {
		"theme": "light",
		"viewBackgroundColor": "#ffffff",
		"currentItemStrokeColor": "#a61e4d",
		"currentItemBackgroundColor": "transparent",
		"currentItemFillStyle": "hachure",
		"currentItemStrokeWidth": 1,
		"currentItemStrokeStyle": "solid",
		"currentItemRoughness": 1,
		"currentItemOpacity": 100,
		"currentItemFontFamily": 1,
		"currentItemFontSize": 16,
		"currentItemTextAlign": "left",
		"currentItemStrokeSharpness": "sharp",
		"currentItemStartArrowhead": null,
		"currentItemEndArrowhead": "arrow",
		"currentItemLinearStrokeSharpness": "round",
		"gridSize": null
	},
	"files": {}
}
```

# SVG snapshot
==⚠ Remove all linebreaks from SVG string before use. Linebreaks were added to improve markdown view speed. ⚠==
```html
<svg version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1021.6295640733507 2015.2223578559028" width="1021.6295640733507" height="2015.2223578559028">
	<!-- svg-source:excalidraw -->

	<defs>
		<style>
			@font-face {
				font-family: "Virgil";
				src: url("https://excalidraw.com/Virgil.woff2");
			}

			@font-face {
				font-family: "Cascadia";
				src: url("https://excalidraw.com/Cascadia.woff2");
			}
		</style>
	</defs>
	<g transform="translate(10 152.99993726942301) rotate(0 449.5 17.5)"><text x="0" y="25" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">AIM: Generate good input that can go deeper (pass the parser)</text></g>
	<g transform="translate(11.66668701171875 223.33326551649333) rotate(0 171.5 17.5)"><text x="0" y="25" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">Case Study: PDF format</text></g>
	<g transform="translate(514.6295640733507 337.88886176215294) rotate(0 248.5 9.5)"><text x="0" y="16" font-family="Cascadia, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">obj_id obj_ver START_FLAG f3728hq89f2@j89fw END_FLAG </text></g>
	<g stroke-linecap="round">
		<g transform="translate(566.377134295228 444.0133617691142) rotate(0 103.99078587811711 -42.27415860297045)">
			<path d="M0.2 0.86 C34.56 -13.37, 172.66 -70.23, 207.22 -84.53 M-1.15 0.27 C33.5 -13.83, 174.91 -68.96, 209.54 -82.99" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(566.377134295228 444.0133617691142) rotate(0 103.99078587811711 -42.27415860297045)">
			<path d="M188.55 -64.32 C195.68 -71.44, 203.65 -79.78, 210.35 -82.08 M188.01 -62.7 C192.3 -68.15, 199.05 -74.25, 210.12 -83.16" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(566.377134295228 444.0133617691142) rotate(0 103.99078587811711 -42.27415860297045)">
			<path d="M180.99 -83.4 C191.19 -83.14, 202.08 -84.08, 210.35 -82.08 M180.44 -81.78 C186.74 -82.41, 195.42 -83.65, 210.12 -83.16" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(564.8166307233262 445.888910105313) rotate(0 108.5 10)"><text x="0" y="14" font-family="Virgil, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">can contain array, dict, ref</text></g>
	<g stroke-linecap="round" transform="translate(87.81824951403416 320.6672236693677) rotate(0 113.2394786783348 42.80861757077875)">
		<path d="M0.89 1.23 C64.65 0.29, 126.38 1.89, 225.19 0.5 M0.53 -0.56 C50.14 -0.39, 99.26 -1.5, 226.79 -0.34 M225.61 -1.25 C226.6 32.92, 227.4 61.39, 228.07 86.12 M225.62 0.43 C226.75 20.56, 227.18 42.92, 226.54 86.55 M226.13 85.03 C174.97 84.19, 121.6 84.31, 1.49 85.4 M226.04 85.95 C178.43 86.81, 130.2 86.84, 0.6 86.17 M1.09 85.89 C-1.75 61.04, 0.31 38.05, -0.58 0.93 M-0.97 85.89 C-0.07 65.8, 0.63 45.03, -0.98 -0.48" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(106.94862415762253 332.35797573974367) rotate(0 36.5 10)"><text x="0" y="14" font-family="Virgil, Segoe UI Emoji" font-size="15.941978869656875px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">PDF body</text></g>
	<g transform="translate(135.20639827888766 359.8918717978851) rotate(0 22.5 7.5)"><text x="0" y="11" font-family="Virgil, Segoe UI Emoji" font-size="11.946595189300496px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">Objects</text></g>
	<g transform="translate(137.3425781548433 381.8935149000039) rotate(0 63.5 7.5)"><text x="63.5" y="11" font-family="Virgil, Segoe UI Emoji" font-size="11.946595189300487px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Cross-reference-table</text></g>
	<g transform="translate(200.95816107975307 359.59319780362375) rotate(0 20 7.5)"><text x="20" y="11" font-family="Virgil, Segoe UI Emoji" font-size="11.94659518930048px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Trailer</text></g>
	<g stroke-linecap="round" transform="translate(87.67319314170607 415.04533477937053) rotate(0 113.2394786783348 42.80861757077875)">
		<path d="M1.19 -1.15 C66.78 0.69, 134.57 -1.25, 225.49 -0.03 M-0.6 -0.58 C52.99 -1.92, 104.84 -1.43, 227.16 -0.52 M227.63 -1.74 C226.23 23.39, 225.79 49.26, 227.33 87.28 M226.13 -0.92 C226.42 21.92, 226.08 42.14, 225.54 84.73 M227.97 84.55 C164.65 86.71, 103.93 85.5, 0.41 85.85 M225.97 86.23 C180.41 85.69, 132.16 85.43, 0.15 85.26 M-0.02 85.56 C-1.72 62.13, -0.64 39.78, -0.82 -0.79 M-0.63 85.77 C1.71 56.79, 0.66 29.05, 0.47 -0.13" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(106.80356778529375 426.73608684974636) rotate(0 36.5 10)"><text x="0" y="14" font-family="Virgil, Segoe UI Emoji" font-size="15.941978869656875px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">PDF body</text></g>
	<g transform="translate(135.06134190655922 454.26998290788765) rotate(0 22.5 7.5)"><text x="0" y="11" font-family="Virgil, Segoe UI Emoji" font-size="11.946595189300496px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">Objects</text></g>
	<g transform="translate(137.19752178251446 476.27162601000657) rotate(0 63.5 7.5)"><text x="63.5" y="11" font-family="Virgil, Segoe UI Emoji" font-size="11.946595189300487px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Cross-reference-table</text></g>
	<g transform="translate(200.81310470742446 453.9713089136263) rotate(0 20 7.5)"><text x="20" y="11" font-family="Virgil, Segoe UI Emoji" font-size="11.94659518930048px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Trailer</text></g>
	<g stroke-linecap="round" transform="translate(68.7036607530381 306.33130032767815) rotate(0 205.48145548502606 108.31580701660809)">
		<path d="M0.11 -0.32 C161.71 -2.06, 323.75 -1.66, 409.93 -0.52 M0.03 0.5 C154.47 1.63, 309.7 1.68, 410.9 -0.44 M409.91 -1.66 C410.19 81.47, 409.84 165.27, 410.74 215.67 M410.53 -0.23 C411.87 75.98, 410.4 152.53, 411.42 216.44 M410.48 216.37 C291.99 215.62, 170.01 215.71, 0.02 215.98 M411.29 216.19 C247.81 215.61, 85.4 215.9, -0.2 216.8 M0.24 217.63 C-0.66 140.62, 0.05 66.09, 0.05 0.46 M0.17 216.4 C-1.53 159.88, -1.57 102.11, -0.15 -0.82" stroke="#087f5b" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(351.638868972343 457.8539705792067) rotate(0 44.5 13.5)"><text x="0" y="20" font-family="Virgil, Segoe UI Emoji" font-size="21.503871340740883px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">PDF File</text></g>
	<g stroke-linecap="round">
		<g transform="translate(295.3347000467088 553.4438378539612) rotate(0 51.12864816333879 -32.86168053622279)">
			<path d="M0.35 -1.14 C17.46 -11.99, 84.58 -55.04, 101.66 -65.89 M-0.92 0.88 C16.67 -9.72, 86.85 -53.43, 104.06 -64.43" stroke="#a61e4d" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(295.3347000467088 553.4438378539612) rotate(0 51.12864816333879 -32.86168053622279)">
			<path d="M83.77 -41.73 C89.54 -44.99, 93.03 -52.69, 102.17 -65.04 M85.37 -40.96 C91.35 -47.3, 95.21 -54.53, 103.55 -64.43" stroke="#a61e4d" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(295.3347000467088 553.4438378539612) rotate(0 51.12864816333879 -32.86168053622279)">
			<path d="M72.84 -59.09 C80.87 -58.62, 86.73 -62.55, 102.17 -65.04 M74.44 -58.33 C83.49 -60.07, 90.28 -62.64, 103.55 -64.43" stroke="#a61e4d" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(21.148098415798472 556.6295844184028) rotate(0 237.5 35.5)"><text x="0" y="24.5" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">IDEA: Use a RNN model to </text><text x="0" y="60" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">Learn the PDF gramma of objects</text></g>
	<g transform="translate(46.03705512152732 692.0554199218755) rotate(0 270.5 28)"><text x="0" y="14.666666666666668" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">In this paper, we investigate how to leverage and adapt</text><text x="0" y="33.333333333333336" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">neural-network-based learning techniques to learn a grammar for non-binary</text><text x="0" y="52" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">PDF data objects.</text></g>
	<g transform="translate(47.518446180555316 781.2221544053822) rotate(0 284.5 37.5)"><text x="0" y="14.75" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">Rules for defining and composing such data objects makes</text><text x="0" y="33.5" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">the bulk of the 1,300-pages PDF-format specification. These rules are numerous</text><text x="0" y="52.25" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">and tedious, but repetitive and structured, and therefore well-suited for learning</text><text x="0" y="71" font-family="Helvetica, Segoe UI Emoji" font-size="16px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">with neural networks (as we will show later).</text></g>
	<g stroke-linecap="round">
		<g transform="translate(726.1381239131172 754.2776421440974) rotate(0 -51.53206108850304 19.271583623784494)">
			<path d="M0.39 -0.17 C-16.7 6.44, -84.89 33.07, -102.26 39.45 M-0.86 -1.3 C-18.07 5, -86.04 31.06, -102.78 37.8" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(726.1381239131172 754.2776421440974) rotate(0 -51.53206108850304 19.271583623784494)">
			<path d="M-80.54 19.32 C-86.12 23.27, -95.74 32.47, -102.19 35.91 M-81.24 18.2 C-88.54 23.47, -95.34 30, -102.51 37.17" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(726.1381239131172 754.2776421440974) rotate(0 -51.53206108850304 19.271583623784494)">
			<path d="M-73.07 38.43 C-81.04 36.19, -93.1 39.17, -102.19 35.91 M-73.77 37.31 C-83.34 36.41, -92.55 36.78, -102.51 37.17" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(724.5553927951385 726.3147243923613) rotate(0 65 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">why NN works</text></g>
	<g transform="translate(28.259209526909558 913.3333333333335) rotate(0 122 17.5)"><text x="0" y="25" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">Sampling Strategy</text></g>
	<g transform="translate(88.99997287326346 978.1481255425347) rotate(0 91 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#364fc7" text-anchor="start" style="white-space: pre;" direction="ltr">1. no-sampling: MAP</text></g>
	<g transform="translate(155.66670735677047 1030.7407294379339) rotate(0 111.5 11.5)"><text x="0" y="18" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">argmax Pr(x[k+1] | x[1:k])</text></g>
	<g transform="translate(86.40733506944406 1075.6666666666667) rotate(0 52.5 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#364fc7" text-anchor="start" style="white-space: pre;" direction="ltr">2. sampling</text></g>
	<g transform="translate(38.9999728732634 10) rotate(0 424.5 22.5)"><text x="0" y="32" font-family="Virgil, Segoe UI Emoji" font-size="36px" fill="#5f3dc4" text-anchor="start" style="white-space: pre;" direction="ltr">Learn&amp;Fuzz: Machine Learning for Input Fuzzing</text></g>
	<g transform="translate(170.481499565972 1131.8889227973093) rotate(0 81.5 11.5)"><text x="0" y="18" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">x[k] ~ Pr( · | x[1:k])</text></g>
	<g transform="translate(170.8518473307288 1219.6297200520835) rotate(0 297.5 22.5)"><text x="0" y="18.5" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">It samples the distribution to generate the next character only when</text><text x="0" y="41" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">the current prefix sequence ends with a whitespace</text></g>
	<g transform="translate(87.98149956597189 1172.4259711371528) rotate(0 77.5 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#364fc7" text-anchor="start" style="white-space: pre;" direction="ltr">3. sample-space</text></g>
	<g transform="translate(21.814765082464987 1318.5370144314238) rotate(0 88 17.5)"><text x="0" y="25" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">Observations</text></g>
	<g transform="translate(104.92587619357596 1377.7777777777778) rotate(0 417 45.50000000000006)"><text x="0" y="17.75" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">A perfect learning technique would always generate well-formed</text><text x="0" y="40.5" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">objects that would not exercise any error-handling code, </text><text x="0" y="63.25" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">whereas a bad learning technique would result in ill-formed objects that will be quickly rejected</text><text x="0" y="86" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">by the parser upfront</text></g>
	<g transform="translate(102.33330620659689 1498.1481933593752) rotate(0 234.5 25.5)"><text x="0" y="18.5" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">too good -&gt; no-error (exercise the same path)</text><text x="0" y="44" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">too bad -&gt; can't go deeper</text></g>
	<g transform="translate(173.96289062499955 1636.0186089409724) rotate(0 54.5 11.5)"><text x="0" y="18" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">Pr( · | x[1:k])</text></g>
	<g stroke-linecap="round">
		<g transform="translate(290.7101443887443 1649.399405072321) rotate(0 149.86314860634047 1.1517014651856243)">
			<path d="M-0.86 0.83 C48.95 1.15, 248.65 0.98, 298.84 1.32 M0.89 0.22 C51.07 0.72, 251.27 2.24, 301.09 2.35" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(290.7101443887443 1649.399405072321) rotate(0 149.86314860634047 1.1517014651856243)">
			<path d="M274.23 10.98 C279.71 10.59, 290.73 6.36, 301.99 1.45 M271.95 12.33 C280.89 10.17, 291.24 5.5, 300.43 3.02" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(290.7101443887443 1649.399405072321) rotate(0 149.86314860634047 1.1517014651856243)">
			<path d="M274.33 -9.54 C279.67 -3.8, 290.67 -1.89, 301.99 1.45 M272.06 -8.19 C281.06 -3.97, 291.38 -2.26, 300.43 3.02" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(600.851847330729 1636.6666666666667) rotate(0 47 12)"><text x="0" y="20" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">c, Pr(c)</text></g>
	<g transform="translate(388.5218270229308 1656.6296522352434) rotate(0 31.5 12.5)"><text x="31.5" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">sample</text></g>
	<g stroke-linecap="round">
		<g transform="translate(629.3236874673362 1674.8730762262815) rotate(0 -45.29930396547394 36.96227493507854)">
			<path d="M-0.43 -0.61 C-15.28 11.8, -75.05 62.46, -89.95 74.78 M1.55 1.68 C-13.33 13.74, -75.27 61, -90.56 73.11" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(629.3236874673362 1674.8730762262815) rotate(0 -45.29930396547394 36.96227493507854)">
			<path d="M-75.59 48.8 C-76.76 54.8, -82.87 57.79, -88.99 73.02 M-74.16 46.97 C-79.1 55.29, -82.31 61.11, -90.06 73.49" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(629.3236874673362 1674.8730762262815) rotate(0 -45.29930396547394 36.96227493507854)">
			<path d="M-63.01 65.02 C-67.12 67.23, -76.13 66.49, -88.99 73.02 M-61.57 63.18 C-69.69 67.44, -76.11 69.12, -90.06 73.49" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round" transform="translate(331.22226291232613 1731.59270562066) rotate(0 129.25923665364584 51.48149278428809)">
		<path d="M130.71 -1.39 C168.38 13.45, 204.72 28.34, 259.27 51.31 M130.37 0.48 C169.83 16.64, 210.06 32.59, 258.1 52.19 M258.64 53.05 C214.87 70.89, 174.85 85.91, 131.64 101.23 M258.97 51.09 C217.48 68.22, 176.3 84.65, 129.32 103.34 M129.65 101.44 C98.9 87.28, 63.58 75.24, -0.3 52.47 M130.17 102.6 C80.8 84.18, 31.97 66, -0.81 52.83 M-1.83 51.91 C41.89 35.66, 88.43 15.17, 128.26 -0.22 M-0.38 51.9 C49.53 32.58, 97.43 11.71, 130.37 0.36" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(389.8889295789928 1769.833435058594) rotate(0 79.5 11.5)"><text x="79.5" y="18" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Pr(c) &gt; threshold?</text></g>
	<g stroke-linecap="round">
		<g transform="translate(363.3679492506411 1757.821799190351) rotate(0 -60.959148940395764 -45.33680291548785)">
			<path d="M-0.6 1 C-20.58 -14.1, -100.73 -75.26, -120.98 -90.4 M1.29 0.47 C-18.68 -15.01, -100.73 -77.01, -121.44 -92.37" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(363.3679492506411 1757.821799190351) rotate(0 -60.959148940395764 -45.33680291548785)">
			<path d="M-91.07 -82.11 C-98.31 -85.97, -103.38 -85.85, -122.11 -90.7 M-91.96 -82.96 C-100 -84.96, -105.25 -86.9, -122.18 -91.74" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(363.3679492506411 1757.821799190351) rotate(0 -60.959148940395764 -45.33680291548785)">
			<path d="M-103.38 -65.69 C-108.06 -72.94, -110.54 -76.27, -122.11 -90.7 M-104.28 -66.54 C-109.65 -72.16, -112.12 -77.81, -122.18 -91.74" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(222.3333062065969 1714.8148600260417) rotate(0 52.5 11.5)"><text x="0" y="18" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">Y: resample</text></g>
	<g stroke-linecap="round">
		<g transform="translate(460.3102501681277 1841.5722071896998) rotate(0 0.48241857561200874 69.08431198674725)">
			<path d="M-0.05 0.5 C0.21 23.34, 1.42 115.07, 1.6 138.16 M-1.54 -0.28 C-1.39 23.27, 0.68 116.79, 0.99 139.69" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(460.3102501681277 1841.5722071896998) rotate(0 0.48241857561200874 69.08431198674725)">
			<path d="M-8.95 112.75 C-6.26 120.31, -1.59 130.24, 2.13 140.89 M-9.92 111.39 C-7.25 118.49, -3.74 124.92, 1.7 140.32" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(460.3102501681277 1841.5722071896998) rotate(0 0.48241857561200874 69.08431198674725)">
			<path d="M11.57 112.37 C8.02 120.05, 6.45 130.09, 2.13 140.89 M10.59 111.02 C8.19 118.35, 6.63 124.87, 1.7 140.32" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(422.33330620659694 1982.2223578559028) rotate(0 32 11.5)"><text x="0" y="18" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">x[k] = c</text></g>
	<g transform="translate(542.7036539713538 1847.037082248264) rotate(0 215.99999999999997 20)"><text x="0" y="14" font-family="Virgil, Segoe UI Emoji" font-size="16px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">IDEA: if the model is too confident of a sample then </text><text x="0" y="34" font-family="Virgil, Segoe UI Emoji" font-size="16px" fill="#a61e4d" text-anchor="start" style="white-space: pre;" direction="ltr">the sample result may be a travail one</text></g>
</svg>
```
%%