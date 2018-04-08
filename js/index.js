import Elm from "../src/Main.elm";

const app = Elm.Main.fullscreen({
    translations: JSON.parse(localStorage.getItem('translations') || '{}'),
});

app.ports.storeTranslations.subscribe(it => {
    console.log('app.ports.storeTranslations ->', it);
    localStorage.setItem('translations', JSON.stringify(it));
});
