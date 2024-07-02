import React, { useState } from "react";
import './App.css';
import { abbrviate, dom, fund, less_than, term_to_string } from "./code";
import { Scanner } from "./parse";
import ReactMarkdown from 'react-markdown';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import 'katex/dist/katex.min.css';

type Operation = "fund" | "dom" | "less_than";

type Options = {
  checkOnOffo: boolean;
  checkOnOffO: boolean;
  checkOnOffA: boolean;
  checkOnOffB: boolean;
  checkOnOffC: boolean;
  checkOnOffT: boolean;
  showHide: boolean;
};

export const headname: string = "茸";
export const headnamereplace: string = "k";

function App() {
  const [inputstrA, setInputA] = useState("");
  const [inputstrB, setInputB] = useState("");
  const [Output, setOutput] = useState("出力：");
  const [outputError, setOutputError] = useState("");
  const [options, setOptions] = useState<Options>({
    checkOnOffo: false,
    checkOnOffO: false,
    checkOnOffA: false,
    checkOnOffB: false,
    checkOnOffC: false,
    checkOnOffT: false,
    showHide: false,
  });

  const compute = (operation: Operation) => {
    setOutput("");
    setOutputError("");
    try {
      const x = new Scanner(inputstrA).parse_term();
      const y = inputstrB ? new Scanner(inputstrB).parse_term() : null;
      const checkarr = [
        options.checkOnOffo,
        options.checkOnOffO,
        options.checkOnOffA,
        options.checkOnOffB,
        options.checkOnOffC,
        options.checkOnOffT,
      ];

      let result;
      switch (operation) {
        case "fund":
          if (y === null) throw new Error("Bの入力が必要です");
          result = fund(x, y);
          break;
        case "dom":
          result = dom(x);
          break;
        case "less_than":
          if (y === null) throw new Error("Bの入力が必要です");
          setOutput(`出力：${less_than(x, y) ? "真" : "偽"}`);
          return;
        default:
          throw new Error("不明な操作");
      }

      const outputString = abbrviate(term_to_string(result, checkarr), checkarr);
      setOutput(`出力：${options.checkOnOffT ? `$${outputString}$` : outputString}`);
    } catch (error) {
      if (error instanceof Error) setOutputError(error.message);
      else setOutputError("不明なエラー");
    }
  };

  const handleCheckboxChange = (key: keyof Options) => {
    setOptions((prevOptions) => ({
      ...prevOptions,
      [key]: !prevOptions[key],
    }));
  };

  return (
    <div className="Appb">
      <h1 className="App">横茸関数計算機</h1>
      <p className="Appa">
        入力は{headname}(a,b), {headname}_&#123;a&#125;(b)の形式で行ってください。<br />
        a=0の時は{headname}(b)としても大丈夫です。<br />
        _, &#123;, &#125;は省略可能です。<br />
        略記として、1 := {headname}(0,0), n := 1 + 1 + ...(n個の1)... + 1, ω := {headname}(0,1), Ω := {headname}(1,0)が使用可能。<br />
        また、{headname}は"{headnamereplace}"で、ωはwで、ΩはWで代用可能。
      </p>
      <div className="block">
        A:
        <input
          id="inputA"
          className="input is-primary"
          value={inputstrA}
          onChange={(e) => setInputA(e.target.value)}
          type="text"
          placeholder="入力A"
        />
        B:
        <input
          id="inputB"
          className="input is-primary"
          value={inputstrB}
          onChange={(e) => setInputB(e.target.value)}
          type="text"
          placeholder="入力B"
        />
      </div>
      <div className="block">
        <button onClick={() => compute("fund")} className="button is-primary">
          A[B]を計算
        </button>
        <button onClick={() => compute("dom")} className="button is-primary">
          dom(A)を計算
        </button>
        <button onClick={() => compute("less_than")} className="button is-primary">
          A &lt; Bか判定
        </button>
      </div>
      <input type="button" value="オプション" onClick={() => handleCheckboxChange('showHide')} className="button is-primary is-light is-small" />
      {options.showHide ? (
        <ul>
          <li><label className="checkbox">
            <input type="checkbox" checked={options.checkOnOffo} onChange={() => handleCheckboxChange('checkOnOffo')} />
            {headname}(0,1)をωで出力
          </label></li>
          <li><label className="checkbox">
            <input type="checkbox" checked={options.checkOnOffO} onChange={() => handleCheckboxChange('checkOnOffO')} />
            {headname}(1,0)をΩで出力
          </label></li>
          <li><label className="checkbox">
            <input type="checkbox" checked={options.checkOnOffA} onChange={() => handleCheckboxChange('checkOnOffA')} />
            {headname}(a,b)を{headname}_a(b)で表示
          </label></li>
          {options.checkOnOffA ? (
            <li><ul><li><label className="checkbox">
              <input type="checkbox" checked={options.checkOnOffB} onChange={() => handleCheckboxChange('checkOnOffB')} />
              全ての&#123; &#125;を表示
            </label></li></ul></li>
          ) : (
            <></>
          )}
          <li><label className="checkbox">
            <input type="checkbox" checked={options.checkOnOffC} onChange={() => handleCheckboxChange('checkOnOffC')} />
            {headname}(0,b)を{headname}(b)で表示
          </label></li>
          <li><label className="checkbox">
            <input type="checkbox" checked={options.checkOnOffT} onChange={() => handleCheckboxChange('checkOnOffT')} />
            TeXで出力
          </label></li>
        </ul>
      ) : (
        <></>
      )}
      <div className="box is-primary">
        {outputError !== "" ? (
          <div className="notification is-danger">{outputError}</div>
        ) : (
          <div className="Appa">
            <ReactMarkdown
              remarkPlugins={[remarkMath]}
              rehypePlugins={[rehypeKatex]}
            >
              {Output}
            </ReactMarkdown>
          </div>
        )}
      </div>
      <p>
        <a href="https://googology.fandom.com/ja/wiki/%E3%83%A6%E3%83%BC%E3%82%B6%E3%83%BC%E3%83%96%E3%83%AD%E3%82%B0:%E7%AB%B9%E5%8F%96%E7%BF%81/%E7%AB%B9%E3%80%81%E8%8C%B8%E3%80%82#%E6%A8%AA%E8%8C%B8%E9%96%A2%E6%95%B0" target="_blank" rel="noreferrer">Definition of "Side Mushroom Function"</a> by <a href="https://googology.fandom.com/ja/wiki/%E3%83%A6%E3%83%BC%E3%82%B6%E3%83%BC:%E7%AB%B9%E5%8F%96%E7%BF%81" target="_blank" rel="noreferrer">竹取翁</a>, Retrieved 2023/08/06 <br />
        The program <a href="https://github.com/SanukiMiyatsuko/side_mushroom" target="_blank" rel="noreferrer">https://github.com/SanukiMiyatsuko/side_bamboo</a> is licensed by <a href="https://creativecommons.org/licenses/by-sa/3.0/legalcode" target="_blank" rel="noreferrer">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.<br />
        Last updated: 2024/06/30
      </p>
    </div>
  );
}

export default App;