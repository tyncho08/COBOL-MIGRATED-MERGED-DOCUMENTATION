'use client'

import { useState, useEffect } from 'react'
import { 
  ExclamationTriangleIcon,
  BellIcon,
  ShoppingCartIcon,
  TruckIcon,
  ClockIcon,
  CheckCircleIcon,
  XCircleIcon,
  ArrowTrendingDownIcon,
  ArrowTrendingUpIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import { formatCurrency, formatDate } from '@/lib/utils'

interface StockAlert {
  id: number
  type: 'low_stock' | 'out_of_stock' | 'overstock' | 'expiring' | 'reorder_point'
  severity: 'critical' | 'warning' | 'info'
  item_code: string
  item_description: string
  current_qty: number
  threshold: number
  location: string
  created_date: string
  action_required: string
  value_impact: number
}

export default function StockAlertsPage() {
  const [alerts, setAlerts] = useState<StockAlert[]>([])
  const [loading, setLoading] = useState(true)
  const [filter, setFilter] = useState<'all' | 'critical' | 'warning' | 'info'>('all')
  const [activeTab, setActiveTab] = useState<'active' | 'resolved'>('active')

  useEffect(() => {
    fetchAlerts()
  }, [])

  const fetchAlerts = async () => {
    try {
      setLoading(true)
      
      // Fetch stock data and generate alerts based on levels
      const response = await fetch('http://localhost:8000/api/v1/stock/cobol/items')
      if (response.ok) {
        const data = await response.json()
        
        // Generate alerts from stock data
        const generatedAlerts: StockAlert[] = []
        let alertId = 1
        
        data.items.forEach((item: any) => {
          const qty = item.quantity || 0
          const reorderPoint = item.reorder_point || 10
          
          // Out of stock alert
          if (qty === 0) {
            generatedAlerts.push({
              id: alertId++,
              type: 'out_of_stock',
              severity: 'critical',
              item_code: item.item_code,
              item_description: item.description,
              current_qty: qty,
              threshold: 0,
              location: item.location || 'MAIN',
              created_date: new Date().toISOString().split('T')[0],
              action_required: 'Urgent: Place order immediately',
              value_impact: item.unit_cost * reorderPoint * 2
            })
          }
          // Low stock alert
          else if (qty > 0 && qty <= reorderPoint) {
            generatedAlerts.push({
              id: alertId++,
              type: 'low_stock',
              severity: 'warning',
              item_code: item.item_code,
              item_description: item.description,
              current_qty: qty,
              threshold: reorderPoint,
              location: item.location || 'MAIN',
              created_date: new Date(Date.now() - 2 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
              action_required: 'Review and place order',
              value_impact: item.unit_cost * (reorderPoint - qty)
            })
          }
          // Overstock alert (if quantity > 3x reorder point)
          else if (qty > reorderPoint * 3) {
            generatedAlerts.push({
              id: alertId++,
              type: 'overstock',
              severity: 'info',
              item_code: item.item_code,
              item_description: item.description,
              current_qty: qty,
              threshold: reorderPoint * 3,
              location: item.location || 'MAIN',
              created_date: new Date(Date.now() - 5 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
              action_required: 'Consider promotion or transfer',
              value_impact: item.unit_cost * (qty - reorderPoint * 2)
            })
          }
        })
        
        setAlerts(generatedAlerts)
      } else {
        console.error('Failed to fetch alerts:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch alerts:', error)
    } finally {
      setLoading(false)
    }
  }

  const getAlertIcon = (type: StockAlert['type']) => {
    switch (type) {
      case 'out_of_stock':
        return <XCircleIcon className="h-5 w-5" />
      case 'low_stock':
        return <ArrowTrendingDownIcon className="h-5 w-5" />
      case 'overstock':
        return <ArrowTrendingUpIcon className="h-5 w-5" />
      case 'expiring':
        return <ClockIcon className="h-5 w-5" />
      case 'reorder_point':
        return <ShoppingCartIcon className="h-5 w-5" />
      default:
        return <ExclamationTriangleIcon className="h-5 w-5" />
    }
  }

  const getSeverityColor = (severity: StockAlert['severity']) => {
    switch (severity) {
      case 'critical':
        return 'text-red-600 bg-red-50 border-red-200'
      case 'warning':
        return 'text-yellow-600 bg-yellow-50 border-yellow-200'
      case 'info':
        return 'text-blue-600 bg-blue-50 border-blue-200'
    }
  }

  const filteredAlerts = alerts.filter(alert => 
    filter === 'all' || alert.severity === filter
  )

  const stats = {
    total: alerts.length,
    critical: alerts.filter(a => a.severity === 'critical').length,
    warning: alerts.filter(a => a.severity === 'warning').length,
    info: alerts.filter(a => a.severity === 'info').length,
    totalValue: alerts.reduce((sum, alert) => sum + alert.value_impact, 0)
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Stock Alerts"
        description="Monitor inventory alerts and take action"
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Stock Control', href: '/stock' },
          { label: 'Alerts' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Alert Summary */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
          <Card 
            className={`cursor-pointer ${filter === 'all' ? 'ring-2 ring-blue-600' : ''}`}
            onClick={() => setFilter('all')}
          >
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <BellIcon className="h-6 w-6 text-gray-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Total Alerts</p>
                  <p className="text-2xl font-semibold text-gray-900">{stats.total}</p>
                </div>
              </div>
            </div>
          </Card>

          <Card 
            className={`cursor-pointer ${filter === 'critical' ? 'ring-2 ring-red-600' : ''}`}
            onClick={() => setFilter('critical')}
          >
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <ExclamationTriangleIcon className="h-6 w-6 text-red-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Critical</p>
                  <p className="text-2xl font-semibold text-red-600">{stats.critical}</p>
                </div>
              </div>
            </div>
          </Card>

          <Card 
            className={`cursor-pointer ${filter === 'warning' ? 'ring-2 ring-yellow-600' : ''}`}
            onClick={() => setFilter('warning')}
          >
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <ExclamationTriangleIcon className="h-6 w-6 text-yellow-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Warnings</p>
                  <p className="text-2xl font-semibold text-yellow-600">{stats.warning}</p>
                </div>
              </div>
            </div>
          </Card>

          <Card>
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <CurrencyDollarIcon className="h-6 w-6 text-green-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Value Impact</p>
                  <p className="text-2xl font-semibold text-gray-900">{formatCurrency(stats.totalValue)}</p>
                </div>
              </div>
            </div>
          </Card>
        </div>

        {/* Tabs */}
        <div className="border-b border-gray-200 mb-6">
          <nav className="-mb-px flex space-x-8">
            <button
              onClick={() => setActiveTab('active')}
              className={`py-2 px-1 border-b-2 font-medium text-sm ${
                activeTab === 'active'
                  ? 'border-blue-500 text-blue-600'
                  : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
              }`}
            >
              Active Alerts ({filteredAlerts.length})
            </button>
            <button
              onClick={() => setActiveTab('resolved')}
              className={`py-2 px-1 border-b-2 font-medium text-sm ${
                activeTab === 'resolved'
                  ? 'border-blue-500 text-blue-600'
                  : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
              }`}
            >
              Resolved (0)
            </button>
          </nav>
        </div>

        {/* Alerts List */}
        <div className="space-y-4">
          {loading ? (
            <Card>
              <div className="p-8 text-center">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
                <p className="mt-4 text-gray-600">Loading alerts...</p>
              </div>
            </Card>
          ) : filteredAlerts.length === 0 ? (
            <Card>
              <div className="p-8 text-center">
                <CheckCircleIcon className="h-12 w-12 text-green-500 mx-auto" />
                <p className="mt-4 text-gray-600">No alerts to display</p>
              </div>
            </Card>
          ) : (
            filteredAlerts.map(alert => (
              <Card key={alert.id} className={`border ${getSeverityColor(alert.severity)}`}>
                <div className="p-6">
                  <div className="flex items-start">
                    <div className={`flex-shrink-0 ${getSeverityColor(alert.severity)} p-2 rounded-lg`}>
                      {getAlertIcon(alert.type)}
                    </div>
                    <div className="ml-4 flex-1">
                      <div className="flex items-start justify-between">
                        <div>
                          <h3 className="text-lg font-medium text-gray-900">
                            {alert.item_description}
                          </h3>
                          <p className="text-sm text-gray-500">
                            Item Code: {alert.item_code} • Location: {alert.location}
                          </p>
                        </div>
                        <div className="text-right">
                          <p className="text-sm text-gray-500">{formatDate(alert.created_date)}</p>
                          <p className="text-lg font-medium text-gray-900">
                            {formatCurrency(alert.value_impact)}
                          </p>
                        </div>
                      </div>
                      
                      <div className="mt-4 grid grid-cols-1 md:grid-cols-3 gap-4">
                        <div>
                          <p className="text-sm text-gray-600">Current Quantity</p>
                          <p className="text-lg font-medium">{alert.current_qty} units</p>
                        </div>
                        <div>
                          <p className="text-sm text-gray-600">Threshold</p>
                          <p className="text-lg font-medium">{alert.threshold} units</p>
                        </div>
                        <div>
                          <p className="text-sm text-gray-600">Action Required</p>
                          <p className="text-sm font-medium text-gray-900">{alert.action_required}</p>
                        </div>
                      </div>
                      
                      <div className="mt-4 flex space-x-3">
                        <Button size="sm" onClick={() => alert(`Create PO for ${alert.item_code}`)}>
                          <ShoppingCartIcon className="h-4 w-4 mr-1" />
                          Create PO
                        </Button>
                        <Button variant="outline" size="sm" onClick={() => alert(`View history for ${alert.item_code}`)}>
                          View History
                        </Button>
                        <Button 
                          variant="outline" 
                          size="sm"
                          onClick={() => {
                            setAlerts(alerts.filter(a => a.id !== alert.id))
                          }}
                        >
                          Dismiss
                        </Button>
                      </div>
                    </div>
                  </div>
                </div>
              </Card>
            ))
          )}
        </div>

        {/* Alert Settings */}
        <Card className="mt-8">
          <div className="p-6">
            <h3 className="text-lg font-medium text-gray-900 mb-4">Alert Settings</h3>
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <div>
                  <p className="font-medium text-gray-900">Low Stock Alerts</p>
                  <p className="text-sm text-gray-500">Notify when items reach reorder point</p>
                </div>
                <Button variant="outline" size="sm">Configure</Button>
              </div>
              <div className="flex items-center justify-between">
                <div>
                  <p className="font-medium text-gray-900">Email Notifications</p>
                  <p className="text-sm text-gray-500">Send daily summary of critical alerts</p>
                </div>
                <Button variant="outline" size="sm">Configure</Button>
              </div>
            </div>
          </div>
        </Card>
      </main>
    </div>
  )
}

// Add missing import
import { CurrencyDollarIcon } from '@heroicons/react/24/outline'