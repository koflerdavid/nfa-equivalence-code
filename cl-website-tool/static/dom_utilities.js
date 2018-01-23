'use strict';

window.DomUtiltities = (function (undefined) {
    function removeChildNodes(parent, selector) {
        if (selector) {
            for (const element of parent.querySelectorAll(selector)) {
                element.parentNode.removeChild(element);
            }
        } else {
            removeImmediateChildNodes(parent);
        }
    }

    function removeImmediateChildNodes(element) {
        while (element.firstChild) {
            element.removeChild(element.firstChild);
        }
    }

    function makeElementVisible(element) {
        element.classList.remove('hidden');
        element.classList.add('visible');
    }

    function hideElement(element) {
        element.classList.remove('visible');
        element.classList.add('hidden');
    }

    function firstParent(element, predicate) {
        for (let parent = element.parentNode; parent != null; parent = parent.parentNode) {
            if (!predicate) {
                return parent;
            }

            if (typeof predicate === 'string' && parent.matches(predicate)) {
                return parent;
            }

            if (typeof predicate === 'function' && predicate.call(null, parent)) {
                return parent;
            }
        }

        return null;
    }

    return {
        removeImmediateChildNodes: removeImmediateChildNodes,
        removeChildNodes: removeChildNodes,
        makeElementVisible: makeElementVisible,
        hideElement: hideElement,
        firstParent: firstParent
    };
})();
