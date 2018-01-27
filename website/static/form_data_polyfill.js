'use strict';

window.FormData = (function (undefined) {
    // This is mostly necessary on some WebKit browsers, for example Safari and Gnome Epiphany
    if (window.FormData.prototype.keys != null) {
        return window.FormData;
    }

    function FormData(form) {
        if (form == null) {
            this.data = new Map();
        } else {
            this.data = parseForm(form);
        }
    }

    FormData.prototype.get = function (name) {
        const values = this.data.get(name);
        return values ? values[0] : null;
    };

    FormData.prototype.getAll = function (name) {
        const values = this.data.get(name);
        return values || [];
    };

    FormData.prototype.keys = function () {
        return this.data.keys();
    };

    function parseForm(form) {
        const formData = new Map();

        for (let element of form.querySelectorAll('input, textarea')) {
            let name = element.getAttribute("name") || null;
            if (name == null) {
                continue;
            }

            if (!formData.has(name)) {
                formData.set(name, []);
            }

            let values = formData.get(name);
            values.push(element.value);
        }

        return formData;
    }

    return FormData;
})(undefined);
