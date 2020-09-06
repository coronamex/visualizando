# Análisis y visualización del número de casos

## Visualizaciones

* **inicio_sintomas_por_fecha.r**: Genera la gráfica básica de número
de casos confirmados a nivel nacional por fecha de inicio de síntomas,
incluyendo la proporción de los casos de cada fecha que han sido
hospitalizados en algún momento.

* **sir.r**: Grafica el número de casos en función del resultado del modelo
SEIR nacional Incluyendo las sub-variantes Centinela, y la gráfica que
muestra el aplanamiento.

* **aplanado_diagrama.r**: Genera diagrama que simula epidemia aplanada
y muestra la poca utilidad de gráficas log-log.

## Análisis

* **centinela_oficial_bayes_seir**: Lee los datos oficiales del sistema
Centinela, ajusta el modelo SEIR a estos datos y produce pronósticos basados
en estos estimados.

* **centinela_coronamex_bayes_seir**: Utiliza los datos de casos en las USMER
que son la base del sistema Centinela, para reconstruir una versión
simplificada de los estimados del sistema de vigilancia Centinela. Luego
ajusta estos estimados a el modelo SEIR y produce pronósticos basados en
estos estimados.

* **stan_seir.r**: Ajusta el modelo SEIR a el número de casos confirmados
nacionales, y produce pronósticos basado en los estimados con los casos
que mostraron síntomas hasta diferentes fechas.

* **sir.stan**: Código de [Stan](https://mc-stan.org) para ajustar el modelo
SEIR con un enfoque bayesiano. Específicamente usand una forma de MCMC llamada
Monte Carlo Hamiltoniano. Es la parte común de todos las versiones del modelo
SEIR y se llama y compila desde R, pero pueden utilizarse cualquiera
de las interfaces de Stan.

## Licencia y derechos de autor
A menos que se indique explícitamente todo el código aquí presentado
fue desarrollado por [Sur Herrera Paredes](https://github.com/surh) y
se se distribuye bajo una licencia de software libre [GPL-3](LICENSE).

    (C) Copyright 2020 Sur Herrera Paredes

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
