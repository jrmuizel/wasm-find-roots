import init, { count_lines } from './pkg/wasm_find_roots.js';

async function run() {
    await init();

    const fileInput = document.getElementById('fileInput');
    const lineCountSpan = document.getElementById('output-result');

    fileInput.addEventListener('change', async (event) => {
        const file = event.target.files[0];
        if (file) {
            const arrayBuffer = await file.arrayBuffer();
            const uint8Array = new Uint8Array(arrayBuffer);
            const lines = count_lines(uint8Array, document.getElementById('target').value);
            lineCountSpan.textContent = lines;
        }
    });
}

run();
