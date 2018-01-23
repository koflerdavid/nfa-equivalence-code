'use strict';

window.setupInputAddon = (function (window, undefined) {
    function setupInputAddon(character, inputField, button) {
        button.addEventListener('click', function () {
            // Insert the character at the cursor position and advance the cursor.
            inputField.setRangeText(character, inputField.selectionStart, inputField.selectionEnd, "end");

            // Bring focus back to the text field
            inputField.focus();
        });
    }

    return setupInputAddon;
})(window);
