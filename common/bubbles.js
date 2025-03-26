const canvas = document.getElementById('bubbledisplay');
const debugDisplay = document.getElementById('mouse_pos');

/**
 * @typedef {Object} BubbleShape
 * @prop {number} x_pos
 * @prop {number} y_pos
 * @prop {number} radius
 * @prop {number} fadeOutAt
 * @prop {number} fadeInAt
 * @prop {{ x: number, y: number, width: number, height: number}} boundingBox
 * @prop {boolean} was_popped
 */

/**
 * Random integer between `min` and `max`
 * @param {number} min
 * @param {number} max
 * @returns {number}
 */
function randomIntBetween(min, max) {
	return Math.floor(Math.random() * (max - min + 1) + min);
}

/** @param {HTMLCanvasElement} canvas */
function resizeCanvasByWindowSize(canvas) {
	canvas.width = window.innerWidth;
	canvas.height = document.body.scrollHeight;
}

/**
 * @param {CanvasRenderingContext2D} ctx
 * @param {number} x
 * @param {number} y
 * @param {number} radius
 * @param {string} color
 */
function drawCircle(ctx, x, y, radius, color) {
	ctx.strokeStyle = '#997AC244';
	ctx.lineWidth = 5;
	ctx.beginPath();
	ctx.arc(x, y, radius, 0, 2 * Math.PI);
	ctx.stroke();
}

/**
 * @param {CanvasRenderingContext2D} ctx
 * @param {HTMLCanvasElement} canvas
 * @param {number} ts
 */
function drawFrame(ctx, canvas, ts) {
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	for (const bubble of bubbles) {
		if (bubble.radius >= 0) drawCircle(ctx, bubble.x_pos, bubble.y_pos, bubble.radius, 'red');
		bubble.y_pos -= 1;
		bubble.x_pos += Math.sin(bubble.y_pos / 40) * 2;

		if (bubble.was_popped && bubble.radius > 0) {
			bubble.radius -= 3;
			continue;
		} else if (bubble.was_popped && bubble.radius === 0) {
			bubbles = bubbles.filter(b => b !== bubble)
			continue;
		}

		if (bubble.radius > -1 && bubble.y_pos < bubble.fadeOutAt) {
			if (bubble.radius === 0) {
				bubbles = bubbles.filter(b => b !== bubble)
				continue;
			}

			bubble.radius -= 1;
		} else if (bubble.y_pos < bubble.fadeInAt && bubble.radius !== bubble.maxRadius) {
			bubble.radius += 1;
		}


		bubble.boundingBox = {
			x: bubble.x_pos - bubble.radius - 2.5,
			y: bubble.y_pos - bubble.radius - 2.5,
			width: (bubble.radius * 2) + 5,
			height: (bubble.radius * 2) + 5
		};
	}
}


let start;
/** @type {BubbleShape[]}  */
let bubbles = [];

let mousePos = {x: 0, y: 0};

const context = canvas.getContext('2d');

function step(ts) {
	if (start === undefined) {
		start = ts;
	}

	const elapsed = ts - start;

	resizeCanvasByWindowSize(canvas);
	drawFrame(context, canvas, elapsed);
	requestAnimationFrame(step);
}

setInterval(() => {
	if (document.hasFocus()) {
		bubbles.push({
			x_pos: randomIntBetween(100, canvas.width - 100),
			y_pos: canvas.height,
			radius: 0,
			maxRadius: randomIntBetween(10, 30),
			fadeOutAt: randomIntBetween(canvas.height / 2, canvas.height / 50),
			fadeInAt: canvas.height - 1,
			boundingBox: {width: 0, height: 0}
		});
	}
}, 1000)

requestAnimationFrame(step);

document.body.addEventListener('click', (event) => {
	for (const bubble of bubbles) {
		if (
			mousePos.x >= bubble.boundingBox.x &&
			mousePos.x <= bubble.boundingBox.x + bubble.boundingBox.width &&
			mousePos.y >= bubble.boundingBox.y &&
			mousePos.y <= bubble.boundingBox.y + bubble.boundingBox.height
		) {
			bubbles.map(b => b === bubble ? b.was_popped = true : b);
			break;
		}
	}
});
document.body.addEventListener("mousemove", (ev) => {
	debugDisplay.innerText = `${ev.x}x${ev.y}`

	mousePos.x = ev.x;
	mousePos.y = ev.y;
}, false);