"""Momentum weights and historical return calculations from the Python prototype."""

import pandas as pd
from joblib import Parallel, delayed


def construct_portfolios(df, start_time, end_time, n, vol_treshold = 0.1):    
    
    start_time = pd.to_datetime(start_time)
    end_time = pd.to_datetime(end_time)
    # Filter the DataFrame for the specified time range
    filtered_df = df.loc[str(start_time):str(end_time)]
    filtered_df_vol = df.loc[str(start_time-pd.Timedelta(days=7)):str(end_time)]
    

    # Calculate the cumulative returns over the time period
    cumulative_returns = (1 + filtered_df).prod() - 1

    # Identify the top n best performing assets
    best_gainers = cumulative_returns.nlargest(n).index
    
    # Identify the top n worst performing assets
    worst_gainers = cumulative_returns.nsmallest(n).index
    
    # Initialize the portfolio classification DataFrame with zeros
    portfolio_classification = pd.DataFrame(0, index=[0], columns=df.columns).astype(float)
    
    # Assign 1 to best performers and -1 to worst performers
    portfolio_classification.loc[0, best_gainers] = float(1/n) if filtered_df.sum(axis = 1).std() < vol_treshold else 0
    portfolio_classification.loc[0, worst_gainers] = float(-1/n) if filtered_df.sum(axis = 1).std() < vol_treshold else 0
    
    return portfolio_classification.squeeze(axis=0)


def test_strategy(df, start_time, end_time, n, t,vol_treshold = 0.1):
    # Construct portfolios based on the training period
    start_time = pd.to_datetime(start_time)
    end_time = pd.to_datetime(end_time)
    
    weights = construct_portfolios(df, start_time, end_time, n,vol_treshold)

    test_start = end_time + pd.to_datetime(df.index).to_series().diff().tail(1).iloc[0]
    test_end = test_start + pd.Timedelta(hours=t) - pd.to_datetime(df.index).to_series().diff().tail(1).iloc[0]
    
    # Filter the DataFrame for the test period
    test_df = df.loc[str(test_start):str(test_end)]
    
    # Calculate the weighted returns over the test period
    # The weights are 1 for best performers, -1 for worst performers, and 0 for others
    weighted_returns = test_df.mul(weights, axis=1)
    
    # Calculate the total return by summing the weighted returns
    total_return = ((1 + weighted_returns).prod()-1).sum()

    output = pd.Series({'training_start': start_time, 'training_end': end_time, 'test_start': test_start, 'test_end': test_end, 'weights': weights, 'return': total_return})
    
    return output


def momentum_testing(returns_df, training_start, calibration_length, loop_end, n, t,vol_treshold = 0.1):
    training_start = pd.to_datetime(training_start)
    training_end = training_start + pd.Timedelta(hours = calibration_length)
    loop_end = pd.to_datetime(loop_end)
    end = training_start
    series_list = []
    param_list = [[training_start, training_end, n, t]]

    while end <= loop_end:
        # series_list.append(test_strategy(returns_df, training_start, training_end, n, t))
        
        training_start += pd.Timedelta(hours=t)
        training_end += pd.Timedelta(hours=t)
        end = training_end + pd.Timedelta(hours=t)
        params = list([training_start, training_end, n, t])
        param_list.append(params)

    series_list = Parallel(n_jobs=-1)(delayed(test_strategy)(returns_df, params[0], params[1], params[2], params[3],vol_treshold) for params in param_list)

    TEST_RES_DF = pd.DataFrame(series_list)
    # TEST_RES_DF['CumRet'] = (TEST_RES_DF['return']+1).cumprod()

    return(TEST_RES_DF)
